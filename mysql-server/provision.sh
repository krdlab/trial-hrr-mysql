set -eu
if ! [ `which ansible` ]; then
    sudo yum install -y epel-release
    sudo yum update -y
    sudo yum install -y ansible gmp libselinux-python
fi
ansible-playbook /vagrant/mysql.yml --connection=local
