"""
Google search plugin
"""

__kupfer_name__ = _("Google search")
__kupfer_sources__ = ()
__kupfer_actions__ = ("GoogleSearch",)
__description__ = _("Search the web with Google")
__version__ = "1.0"
__author__ = "Benjamin Staffin <benley@gmail.com>"

import urllib.parse

import kupfer
import kupfer.utils


class GoogleSearch(kupfer.objects.Action):

    def __init__(self):
        kupfer.objects.Action.__init__(self, __kupfer_name__)

    def activate(self, leaf):
        kupfer.utils.show_url(
            "https://google.com/search?{}".format(
                urllib.parse.urlencode({"q": leaf.object})))

    def item_types(self):
        yield kupfer.objects.TextLeaf

    def get_description(self):
        return __description__

    def get_icon_name(self):
        return "google-search"
