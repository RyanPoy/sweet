from __future__ import annotations

import sys
from loguru import logger
from pathlib import Path

_default_configured = False
_custom_logger = None


def configure_logger(
        level: str = "DEBUG",
        sink: str | Path | None = sys.stdout,  # 输出位置，可为控制台、文件或自定义
        json_output: bool = False,  # 是否以JSON格式输出
        module: str = "sweet"  # 默认日志模块名称
):
    global _default_configured
    logger.remove()  # 清空已有的 sink 配置

    # 设置日志格式
    fmt = (
        "<green>{time:YYYY-MM-DD HH:mm:ss.SSS}</green> | "
        "<level>{level: <8}</level> | "
        "<cyan>{module}</cyan>.<cyan>{function}</cyan>:<cyan>{line}</cyan> - "
        "<level>{message}</level>"
    )

    # 配置日志输出
    logger.add(
        sink=sink,  # 输出位置（文件、控制台等）
        level=level,  # 日志级别
        format=fmt,  # 格式
        serialize=json_output,  # 是否JSON格式化
        backtrace=True,  # 错误堆栈信息
        diagnose=True,  # 更详细的诊断信息
        enqueue=True  # 启用多线程时队列化处理
    )

    _default_configured = True
    return logger.bind(module=module)


def get_logger(module: str = "sweet"):
    """ 获取模块化的 logger，默认会初始化日志配置 """
    if not _default_configured:
        # 如果日志没有配置，使用默认配置
        configure_logger(module=module)
    return logger.bind(module=module)


def set_custom_logger(custom_logger):
    """ 允许用户传入自定义的 logger """
    global _custom_logger
    _custom_logger = custom_logger
