# Trial Project for Haskell Relational Record (HRR) using MySQL

## はじめに

HRR と MySQL の組み合わせで色々と実験するためのプロジェクト．

* 実行環境
    * GHC >=7.8.3
    * cabal-install >=1.20.0.4
    * vagrant >=1.7.2
        * 以下のプラグインをインストール済み
            * [vagrant-vbguest](https://github.com/dotless-de/vagrant-vbguest)
            * [vagrant-hostsupdater](https://github.com/cogitatio/vagrant-hostsupdater)
    * virtualbox >=4.3.26
* VM 環境
    * CentOS 6.6
    * MySQL >=5.6.24

## 実行

```sh
## VM の起動
$ cd mysql-server && vagrant up
...
[sudo] password for <username>:         # required by vagrant-hostsupdater
...
$ cd -

$ git clone https://github.com/khibino/haskell-relational-record.git hrr
$ git clone https://github.com/bos/hdbc-mysql.git

$ cd examples
$ cabal sandbox init
$ cabal sandbox add-source \
    ../hrr/HDBC-session \
    ../hrr/names-th \
    ../hrr/persistable-record \
    ../hrr/relational-query \
    ../hrr/relational-query-HDBC \
    ../hrr/relational-schemas \
    ../hrr/sql-words \
    ../hdbc-mysql
$ cabal install
$ .cabal-sandbox/bin/hrr-examples
```

## MySQL 固有の事項

ベーシックな使い方に限れば，スキーマ/テーブル名の normalize を抑制すれば良いだけになっている．
(`IGNORE_SPACE` が不要になっていた)

```haskell
import Database.Relational.Query (Config (..), defaultConfig)
config :: Config
config =  defaultConfig { normalizedTableName = False }
```

あとはこの `config` を `defineTableFromDB'` といった `Config` を受け取る関数に渡せばよい．

## 参考情報

* <https://khibino.github.io/haskell-relational-record/>
* <https://github.com/khibino/haskell-relational-record/tree/master/relational-record-examples>
* <https://hackage.haskell.org/package/relational-query>

