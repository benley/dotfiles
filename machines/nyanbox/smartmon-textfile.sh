#!/usr/bin/env bash
# Script informed by the collectd monitoring script for smartmontools (using smartctl)
# by Samuel B. <samuel_._behan_(at)_dob_._sk> (c) 2012
# source at: http://devel.dob.sk/collectd-scripts/

# TODO: This probably needs to be a little more complex.  The raw numbers can have more
#       data in them than you'd think.
#       http://arstechnica.com/civis/viewtopic.php?p=22062211

set -u

SMARTCTL="smartctl"

parse_smartctl_attributes_awk="$(cat << 'SMARTCTLAWK'
$1 ~ /^[0-9]+$/ && $2 ~ /^[a-zA-Z0-9_-]+$/ {
  gsub(/-/, "_");
  printf "%s_value{%s,smart_id=\"%s\"} %d\n", $2, labels, $1, $4
  printf "%s_worst{%s,smart_id=\"%s\"} %d\n", $2, labels, $1, $5
  printf "%s_threshold{%s,smart_id=\"%s\"} %d\n", $2, labels, $1, $6
  printf "%s_raw_value{%s,smart_id=\"%s\"} %e\n", $2, labels, $1, $10
}
SMARTCTLAWK
)"

smartmon_attrs="$(cat << 'SMARTMONATTRS'
airflow_temperature_cel
command_timeout
current_pending_sector
end_to_end_error
erase_fail_count
g_sense_error_rate
hardware_ecc_recovered
host_reads_mib
host_reads_32mib
host_writes_mib
host_writes_32mib
load_cycle_count
media_wearout_indicator
wear_leveling_count
nand_writes_1gib
offline_uncorrectable
power_cycle_count
power_on_hours
program_fail_count
raw_read_error_rate
reallocated_sector_ct
reported_uncorrect
sata_downshift_count
spin_retry_count
spin_up_time
start_stop_count
temperature_celsius
total_lbas_read
total_lbas_written
udma_crc_error_count
unsafe_shutdown_count
workld_host_reads_perc
workld_media_wear_indic
workload_minutes
SMARTMONATTRS
)"
smartmon_attrs="$(echo ${smartmon_attrs} | xargs | tr ' ' '|')"

parse_smartctl_attributes() {
  local disk="$1"
  local disk_type="$2"
  local labels="disk=\"${disk}\",type=\"${disk_type}\""
  local vars="$(echo "${smartmon_attrs}" | xargs | tr ' ' '|')"
  sed 's/^ \+//g' \
    | awk -v labels="${labels}" "${parse_smartctl_attributes_awk}" 2>/dev/null \
    | tr A-Z a-z \
    | grep -E "(${smartmon_attrs})"
}

parse_smartctl_info() {
  local -i smart_available=0 smart_enabled=0 smart_healthy=0
  local disk="$1" disk_type="$2"
  while read line ; do
    info_type="$(echo "${line}" | cut -f1 -d: | tr ' ' '_')"
    info_value="$(echo "${line}" | cut -f2- -d: | sed 's/^ \+//g')"
    case "${info_type}" in
      Model_Family) model_family="${info_value}" ;;
      Device_Model) device_model="${info_value}" ;;
      Serial_Number) serial_number="${info_value}" ;;
      Firmware_Version) fw_version="${info_value}" ;;
      Vendor) vendor="${info_value}" ;;
      Product) product="${info_value}" ;;
      Revision) revision="${info_value}" ;;
      Logical_Unit_id) lun_id="${info_value}" ;;
    esac
    if [[ "${info_type}" == 'SMART_support_is' ]] ; then
      case "${info_value:0:7}" in
        Enabled) smart_enabled=1 ;;
        Availab) smart_available=1 ;;
        Unavail) smart_available=0 ;;
      esac
    fi
    if [[ "${info_type}" == 'SMART_overall-health_self-assessment_test_result' ]] ; then
      case "${info_value:0:6}" in
        PASSED) smart_healthy=1 ;;
      esac
    elif [[ "${info_type}" == 'SMART_Health_Status' ]] ; then
      case "${info_value:0:2}" in
        OK) smart_healthy=1 ;;
      esac
    fi
  done
  if [[ -n "${vendor:-}" ]] ; then
    echo "device_info{disk=\"${disk}\",type=\"${disk_type}\",vendor=\"${vendor}\",product=\"${product}\",revision=\"${revision}\",lun_id=\"${lun_id}\"} 1"
  else
    echo "device_info{disk=\"${disk}\",type=\"${disk_type}\",model_family=\"${model_family:-}\",device_model=\"${device_model:-}\",serial_number=\"${serial_number:-}\",firmware_version=\"${fw_version:-}\"} 1"
  fi
  echo "device_smart_available{disk=\"${disk}\",type=\"${disk_type}\"} ${smart_available}"
  echo "device_smart_enabled{disk=\"${disk}\",type=\"${disk_type}\"} ${smart_enabled}"
  echo "device_smart_healthy{disk=\"${disk}\",type=\"${disk_type}\"} ${smart_healthy}"
}

output_format_awk="$(cat << 'OUTPUTAWK'
BEGIN { v = "" }
v != $1 {
  print "# HELP smartmon_" $1 " SMART metric " $1;
  print "# TYPE smartmon_" $1 " gauge";
  v = $1
}
{print "smartmon_" $0}
OUTPUTAWK
)"

format_output() {
  sort \
  | awk -F'{' "${output_format_awk}"
}

smartctl_version="$("$SMARTCTL" -V | head -n1  | awk '$1 == "smartctl" {print $2}')"

echo "smartctl_version{version=\"${smartctl_version}\"} 1" | format_output

if [[ "$(expr "${smartctl_version}" : '\([0-9]*\)\..*')" -lt 6 ]] ; then
  exit
fi

device_list="$("$SMARTCTL" -n standby --scan-open | awk '/^\/dev/{print $1 "|" $3}')"

for device in ${device_list}; do
  disk="$(echo ${device} | cut -f1 -d'|')"
  type="$(echo ${device} | cut -f2 -d'|')"
  echo "smartctl_run{disk=\"${disk}\",type=\"${type}\"}" $(TZ=UTC date '+%s')
  # Get the SMART information and health
  "$SMARTCTL" -n standby  -i -H -d "${type}" "${disk}" | parse_smartctl_info "${disk}" "${type}"
  # Get the SMART attributes
  "$SMARTCTL" -n standby -A -d "${type}" "${disk}" | parse_smartctl_attributes "${disk}" "${type}"
done | format_output
