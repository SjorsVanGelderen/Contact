MONO=mono
COMPILER=fsharpc
FILES=          	\
Program.fs
#Networking.fs		\

REFERENCES=-r /usr/lib/monogame/MonoGame.Framework.dll -r /usr/lib/monogame/Lidgren.Network.dll

app.exe: ${FILES}
	${COMPILER} --nologo --target:exe --platform:x86 ${REFERENCES} -o $@ ${FILES}
