<document source="temp.txt">
    <comment xml:space="preserve">
        This is a simple reStructuredText file that represents what I would
    <comment xml:space="preserve">
        like the output of transforming my test Python code to be
    <section class="package" id="package-trivial-package" name="package trivial_package">
        <title>
            Package trivial_package
        <section class="module" id="module-trivial-package-init" name="module trivial_package.__init__">
            <title>
                Module trivial_package.__init__
            <block_quote class="docstring">
                <paragraph>
                    A simple docstring.
        <section class="module" id="module-trivial-package-file1" name="module trivial_package.file1">
            <title>
                Module trivial_package.file1
            <block_quote class="docstring">   ## Hmm - not quite right
                <paragraph>
                    This is the first example file. It 
                    <emphasis>
                        does
                     use reStructuredText.
                <paragraph>
                    Attributes:
                <bullet_list bullet="*">
                    <list_item>
                        <paragraph>
                            __docformat__ = "reST" (line 5)
                <paragraph>
                    Import: os (line 7)
            <section class="class" id="class-trivial-package-file1-fred" name="class trivial_package.file1.fred">
                <title>
                    Class trivial_package.file1.Fred
                <field_list>
                    <field>
                        <field_name>
                            line
                        <field_body>
                            <paragraph>
                                9
                            <paragraph class="docstring"> ## Hmm
                                An example class - it announces each instance as it is created.
                <section class="method" id="method-trivial-package-file1-fred-init" name="method trivial_package.file1.fred.__init__">
                    <title>
                        Method trivial_package.file1.Fred.__init__
                    <field_list>
                        <field>
                            <field_name>
                                line
                            <field_body>
                                <paragraph>
                                    13
                        <field>
                            <field_name>
                                parameters
                            <field_body>
                                <paragraph>
                                    self
        <section class="module" id="module-trivial-package-file2" name="module trivial_package.file2">
            <title>
                Module trivial_package.file2
            <block_quote class="docstring">
                <paragraph>
                    This module is 
                    <emphasis>
                        not
                     using reStructuredText for its docstrings.
        <section class="file" id="file-trivial-package-not-python" name="file trivial_package.not_python">
            <title>
                File trivial_package.not_python
            <paragraph>
                (Not a Python module)
        <section class="package" id="package-trivial-package-sub-package" name="package trivial_package.sub_package">
            <title>
                Package trivial_package.sub_package
            <section class="module" id="module-trivial-package-sub-package-init" name="module trivial_package.sub_package.__init__">
                <title>
                    Module trivial_package.sub_package.__init__
                <paragraph>
                    (No documentation)
