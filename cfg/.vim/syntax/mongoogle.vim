" $Id: //depot/ops/monitoring/mongoogle/contrib/vim/mongoogle.vim#2 $
" Vim syntax file
" Put this in your ~/.vim/syntax directory
" Language:     Mongoogle configuration files
" Maintainer:   sundell@google.com
" URL:          http://www.corp.google.com/~sundell/mongoogle.vim
" Last Change:  2005 Aug 24

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Last match is taken!
syn match   mgBlock     "^\(\(Mon\|Alert\)\(Template\|\)Name\|MasterConf\)"
syn match   mgComment   "^\s*#.*$"

syn keyword mgKeyword  agent_type alarm_path applyto
syn keyword mgKeyword  average_over_seconds bgp_peer_ip
syn keyword mgKeyword  clear_alert_var_list cmdstr
syn keyword mgKeyword  cname community connection_close cookie
syn keyword mgKeyword  crashmail crashmailto db
syn keyword mgKeyword  dbconnect_timeout dbhost dblog dbpasswd dbsleep
syn keyword mgKeyword  dbuser debug depends_on deponds_on
syn keyword mgKeyword  disable_percent_check_if_under disable_percentage
syn keyword mgKeyword  disable_time disable_when_dep_is_bad
syn keyword mgKeyword  divide_result_by dns_lookups driveno
syn keyword mgKeyword  email_return_path enable_num expect_regex_list
syn keyword mgKeyword  expr fail_direction fail_if
syn keyword mgKeyword  fuzzy_dep_count fuzzy_dep_percent_failure
syn keyword mgKeyword  gfs_cell hint hostname
syn keyword mgKeyword  imapserver
syn keyword mgKeyword  inn_servertype interval inteval keydir keys
syn keyword mgKeyword  local_command
syn keyword mgKeyword  log_output_only_if_different_from_last_output
syn keyword mgKeyword  log_output_return_codes log_to_db logdir login
syn keyword mgKeyword  mail.template mailcc mailfile mailfrom
syn keyword mgKeyword  mailtemplate mailto max_counter_value
syn keyword mgKeyword  max_status_log_entries minutes-until-repeat
syn keyword mgKeyword  minutes_until_repeat mon_regex mtr_path
syn keyword mgKeyword  no_dependency_status_check offline oid oid_index
syn keyword mgKeyword  ok_return_codes oob_host oob_utc_time
syn keyword mgKeyword  output_to_other password percent_change_seconds
syn keyword mgKeyword  percent_change_threshold ping_cmd
syn keyword mgKeyword  ping_success_regex portnum
syn keyword mgKeyword  reject_regex remote_command replyto require_regex
syn keyword mgKeyword  require_regex2 require_regex3 require_regexp
syn keyword mgKeyword  send_hostname send_string_list service severity
syn keyword mgKeyword  slb smtpfrom smtpheaderfrom smtpserver smtpto
syn keyword mgKeyword  ssh_hostname ssh_identity ssh_path
syn keyword mgKeyword  ssh_user ssl ssl_name taddr template
syn keyword mgKeyword  template_depends_on threshholds thresholds timeout
syn keyword mgKeyword  traceroute traceroute_path type unique_url
syn keyword mgKeyword  unique_var_name unit unreachable_is_ok url use_dns
syn keyword mgKeyword  use_precision user_agent username

syn match mgEquals "="

" Regex highlighting
syn region mgRegex   start="\^" end="\$" skip="\\\$" contains=mgRegexMeta oneline
syn match mgRegexMeta   "[.*^?$()\[\]\|]" contained
syn match mgRegexMeta   "\\." contained

" When we expect a hostnmame, only highlight things that look like FQDNs
" or IP addresses -- relative names are for chumps!
syn match mgHostRule "^\s\+\(ssh_\)\?hostname\s*=\s*.\+$" contains=mgHostname,mgEquals,mgKeyword
syn match mgHostname  contained "\<\(\([a-zA-Z0-9-]\+\.\)\{2,}[a-zA-Z]\{2,4}\|\([1-2]\?\d\{1,2}\.\)\{3}[1-2]\?\d\{1,2}\)\s*$"

" Only highlight valid disable patterns
syn match mgDisableRule "^\s\+disable_time\s*=\s*.\+$" contains=mgDisableTime,mgEquals,mgKeyword
syn match mgDisableTime contained "\<\(\d\{1,2}:\d\{2}-\d\{1,2}:\d\{2}-\d-\d,\s*\)*\d\{1,2}:\d\{2}-\d\{1,2}:\d\{2}-\d-\d\s*$"

" highlight valid integers after certain keywords
syn match mgNumRule "^\s\+\(interval\|timeout\|minutes_until_repeat\|portnum\|max_counter_value\|divide_result_by\)\s*=\s*\d\+\s*$" contains=mgNum,mgEquals,mgKeyword
syn match mgNum     contained   "\<\d\+\>"


" highlight valid types in type =
syn match mgTypeRule "^\s\+type\s*=\s*.\+$" contains=mgType,mgEquals,mgKeyword
syn keyword mgType    contained   http command composite snmp ping agent port smtp2file

" OID highlighting for oid =
syn match mgOidRule "^\s\+oid\s*=\s*.\+$" contains=mgOid,mgEquals,mgKeyword
syn match mgOid contained "[ =]\.\?1\.3\.6\.\(\d\+\.\)\{3,}\d\+"ms=s+1

" included conf files
syn match mgIncludeRule "^include_conf_file\s*=\s*.*$" contains=mgIncludeFile,mgEquals,mgInclude
syn match mgIncludeFile contained "conf/[^ ]\+.conf"
syn keyword mgInclude contained  include_conf_file

" disable_if
syn match mgDisableIfRule "^\s\+disable_if\s*=\s*.\+$" contains=mgDisableIf,mgEquals,mgDisableIfValue
syn keyword mgDisableIf contained disable_if
syn keyword mgDisableIfValue contained any_bad all_bad

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet.
if version >= 508 || !exists("did_mongoogle_syntax_inits")
    if version < 508
        let did_mongoogle_syntax_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    HiLink mgComment    Comment
    HiLink mgBlock      Function
    HiLink mgKeyword    Keyword
    HiLink mgNum        Number
    HiLink mgEquals     Operator
    HiLink mgRegex      String
    HiLink mgRegexMeta  Special
    HiLink mgHostname   Type
    HiLink mgDisableTime Type
    HiLink mgType       Type
    HiLink mgOid        Type
    HiLink mgError      Error
    HiLink mgInclude    Keyword
    HiLink mgIncludeFile Type
    HiLink mgDisableIf  Keyword
    HiLink mgDisableIfValue Type

    delcommand HiLink
endif

let b:current_syntax = "mongoogle"
