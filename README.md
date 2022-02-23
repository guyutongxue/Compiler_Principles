# `sysyc` 《编译原理》实习

## 测试

> https://pku-minic.github.io/online-doc/#/misc-app-ref/environment

### 拉取 Docker 镜像

```sh
docker pull maxxing/compiler-dev
```

### 启动 Docker 容器

```sh
docker run -it --rm -v $(pwd):/root/compiler maxxing/compiler-dev bash
```

在容器内的 Shell 中，键入测试命令。如：

```sh
autotest -koopa -s lv1 /root/compiler
```
