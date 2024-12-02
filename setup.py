from setuptools import setup, find_packages

setup(
    name="sweet",
    version="0.1",
    packages=find_packages(),
    install_requires=[
        # 依赖的包
        'pip~=24.3.1',
        'wheel~=0.41.2',
        'setuptools~=68.2.0',
        'aiomysql~=0.2.0',
        'PyMySQL~=1.1.1',
        'aiosqlite~=0.20.0',
        'packaging~=24.2',
    ],
    license="MIT",  # 指定使用 MIT 许可证
    tests_require=["pytest"],  # 测试依赖
    test_suite="tests",  # 测试套件
    include_package_data=True,
    author="Ryan Poy",
    author_email="ryanpoy@gmail.com",
    description="Sweet Framework",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    url="https://github.com/RyanPoy/sweet",
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",  # 提示使用 MIT 许可证
    ],
)
