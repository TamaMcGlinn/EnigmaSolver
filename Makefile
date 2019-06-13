main: *.adb
	gnatmake *.adb -gnatX

clean:
	rm -f *.o *.ali main
