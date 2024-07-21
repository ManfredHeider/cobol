      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0873                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    30.11.2007                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00285!30.11.07!2.1 ! NEU                       ! KLEEMEIER *
310108* ASL00292!31.01.08!2.2 ! DRVB0888 + DRVB0889 HINZU ! KLEEMEIER *
140508* AAS00756!14.05.08!2.3 ! DRVB0899 HINZU            ! KLEEMEIER *
040310* ASL00344!04.03.10!2.4 ! DRVB0969 HINZU            ! KLEEMEIER *
230610* ARW00069!23.06.10!2.5 ! DRVB0981 HINZU            ! KLEEMEIER *
020211* AVB01103!02.02.11!2.6 ! DRVB0984 HINZU            ! KLEEMEIER *
110811* ASL00390!15.08.11!2.7 ! DRVB1029 HINZU            ! OBER      *
111111* ASL00399!11.11.11!2.8 ! DRVB1032 HINZU            ! GUTFLEISCH*
030212* ASL00420!03.02.12!2.9 ! DRVB1044 HINZU            ! OBER      *
050315* ASL00530!05.03.15!2.10! DRVB1084 + DRVB1085 HINZU ! B.PRETSCH *
050315* AVB01400!18.05.16!2.11! DRVB1139 + DRVB1140 HINZU ! H.WÖLFLIK *
290317* ASL00567!29.03.17!2.12! DRVB1160 HINZU            ! B.PRETSCH *
050517* ASL00569!05.05.17!2.13! DRVB1161 HINZU            ! B.PRETSCH *
020620* VLA-712 !02.06.20!2.14! DRVB1281 HINZU            ! B.PRETSCH *
240822* VLA-4870!24.08.22!ANPASSUNG FÖRDERMASSNAHME IN WBW! THURMANN  *
      *         !        !NEUKUNDE UND EINBAU WBW TEAM    !           *
      *         !        !                                !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-ZJVB0028.
           05  PI-ZJVB0028-FUNKTION            PIC  X(008).
               88 PI-ZJVB0028-DRVB0874              VALUE 'DRVB0874'.
               88 PI-ZJVB0028-DRVB0875              VALUE 'DRVB0875'.
               88 PI-ZJVB0028-DRVB0876              VALUE 'DRVB0876'.
               88 PI-ZJVB0028-DRVB0877              VALUE 'DRVB0877'.
310108         88 PI-ZJVB0028-DRVB0888              VALUE 'DRVB0888'.
310108         88 PI-ZJVB0028-DRVB0889              VALUE 'DRVB0889'.
140508         88 PI-ZJVB0028-DRVB0899              VALUE 'DRVB0899'.
040310         88 PI-ZJVB0028-DRVB0969              VALUE 'DRVB0969'.
230610         88 PI-ZJVB0028-DRVB0981              VALUE 'DRVB0981'.
020211         88 PI-ZJVB0028-DRVB0984              VALUE 'DRVB0984'.
110811         88 PI-ZJVB0028-DRVB1029              VALUE 'DRVB1029'.
111111         88 PI-ZJVB0028-DRVB1032              VALUE 'DRVB1032'.
030212         88 PI-ZJVB0028-DRVB1044              VALUE 'DRVB1044'.
050315         88 PI-ZJVB0028-DRVB1084              VALUE 'DRVB1084'.
050315         88 PI-ZJVB0028-DRVB1085              VALUE 'DRVB1085'.
180516         88 PI-ZJVB0028-DRVB1139              VALUE 'DRVB1139'.
180516         88 PI-ZJVB0028-DRVB1140              VALUE 'DRVB1140'.
290317         88 PI-ZJVB0028-DRVB1160              VALUE 'DRVB1160'.
050517         88 PI-ZJVB0028-DRVB1161              VALUE 'DRVB1161'.
020620         88 PI-ZJVB0028-DRVB1281              VALUE 'DRVB1281'.
240822         88 PI-ZJVB0028-DRVB1360              VALUE 'DRVB1360'.
      *
      ******* ENDE COPY-MEMBER ****************************************