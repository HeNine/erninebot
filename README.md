ErNineBot
=========

IRC bot written in Erlang.

Build
-----

1. Dependencies

    - Rebar (developed with rebar 2.6.0 R15B03 20150619_161736 git 2.6.0)
    - Erlang 18 (developed with erlang-18.0 on Funtoo)

2. Clone repository

    ```
    git clone https://github.com/HeNine/erninebot.git
    ```

3. Create a node

    The environment variable `$CLONED_PROJECT` is the directory you cloned the repository into.

    ```shell
    $ cd $CLONED_PROJECT
    $ cd ./rel
    $ rebar create-node nodeid=erninebot
    ```

    rebar should report `reltool.config` as already present. Don't overwrite it (unless you know what you are doing).

4. Edit configuration

    Modify `rel/files/sys.config` with server details and desired nickname.

    ```erlang
    [
     %% SASL config
     {sasl, [
             {sasl_error_logger, {file, "log/sasl-error.log"}},
             {errlog_type, error},
             {error_logger_mf_dir, "log/sasl"},      % Log directory
             {error_logger_mf_maxbytes, 10485760},   % 10 MB max file size
             {error_logger_mf_maxfiles, 5}           % 5 files max
            ]},
     {erninebot, [
             {server, "127.0.0.1"},                  % Server address
             {port, 6667},                           % Server port
             {nickname, "ErNineBot"},                % Bot nickname
             {channels, ["#ErNineBotTestChannel"]}   % List of channels bot should join
            ]}
    ].
    ```

5. Generate node

    ```shell
    $ cd $CLONED_PROJECT
    $ rebar compile generate
    ```
6. Start node

    ```shell
    $ cd $CLONED_PROJECT
    $ ./rel/erninebot/bin/erninebot start
    ```
