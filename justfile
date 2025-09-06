format:
    fd -e hs | xargs -n 1 -P $(nproc) fourmolu -i -q
