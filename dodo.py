from doit.tools import LongRunning

def run_cmd (name):
    return f"scopes -e -m .src.{name}.aot"

experiments = [
    "tiny-cpu",
    "regex"
]

def task_compile_experiments ():
    for name in experiments:
        yield {
            'basename': f"compile.experiments.{name}",
            'actions': [LongRunning(run_cmd(name))],
            'uptodate': [False]
        }
