import sublime, sublime_plugin, subprocess, threading, time

class Bdo(sublime_plugin.TextCommand):
    def run(self, cmd):
        sublime.active_window().show_input_panel("bdo ", "update", self.execute, None, None)
    def execute(self, cmd):
        output = subprocess.Popen(
            "echo " + cmd + " | nc -w 10 localhost 9090",
            shell=True, stdout=subprocess.PIPE).stdout.read()
        if len(output) > 0 and output != "Sending link update ...\n":
            view = sublime.active_window().new_file()
            edit = view.begin_edit()
            view.insert(edit, 0, output)
            view.end_edit(edit)
