# `sysyc` 《编译原理》实践部分

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
autotest -w wd /root/compiler -riscv
```

## 扩展语法

- 函数声明；
- 赋值**表达式**（返回左值）；逗号表达式；
- 指针；取地址；解地址；
- 字符字面量，仅可见字符和 `'\n'`。

例：

```c
void swap(int* a, int* b);

int main() {
  int a = 42, b = 56;

  swap(&a, &b);

  putint(a), putch(' ');
  putint(b), putch('\n');
  return 0;
}

void swap(int* a, int* b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}
```
