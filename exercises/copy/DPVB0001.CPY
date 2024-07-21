      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:20:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00110000
      *    COPY-MEMBER: DPVB0001       EQF-Berechung                  * 00120000
      *    --------------------------------------------------------   * 00150000
      *    ERSTELLT: M. KUHN / A.Priggen                              * 00180011
      *    DATUM:    26.10.1994                                       * 00190011
      *    STAND:    26.10.1994                                       * 00200011
      *                                                               * 00200011
      ***************************************************************** 00210000
      *    ACHTUNG!!!                                                 * 00200011
      *    ----------                                                 * 00200011
      *                                                               * 00200011
      *    Im Zuge der vereinheitlichten EQF-Berechnung Frankfurt/    * 00200011
      *    Aachen findet diese Copy-Member unter dem Namen PBOM6735   * 00200011
      *    auch in Aachen Verwendung.                                 * 00200011
      *                                                               * 00200011
      *    Bei Änderungen ist die Gruppe DVA-VU (DVAG) in Aachen zu   * 00200011
      *    informieren!                                               * 00200011
      *                                                               * 00200011
      ***************************************************************** 00210000
      *    VERSION   2    04.01.95 13:32:00
      ****************************************************************  00212002
      * AENDERUNGEN:                                                 *  00213002
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00214002
      * -------------------------------------------------------------*  00215002
      *         !26.10.94! 01 ! NEU                       ! Priggen  *  00216011
281194*         !28.11.94!    ! Korrektur EQF-Berechnung  ! Priggen  *  00217002
281194*         !        !    ! gem. Memo v. 28.11.94     !          *  00218002
      *         !04.01.95! 02 ! Produktionsübergabe       ! Giese    *  00219002
      *         !        !    !                           !          *  00219102
      *         !        !    !                           !          *  00219202
      *         !        !    !                           !          *  00219302
      *         !        !    !                           !          *  00219402
      *                                                              *  00219502
      * ENDE-LOGBUCH                                                 *  00219602
      ****************************************************************  00219702
      *                                                                 00220000
      *---------------------------------------------------------------  07730000
       EQF-BERECHNEN SECTION.                                           07740006
      *---------------------------------------------------------------  07750000
      * Übergabe vor Aufruf:                                            07760005
      *                                                                 07770005
      *    Einheiten müssen als Zugang in Z-DWVB0001-Zugang und als     07780005
      *    Abgang in Z-DWVB0001-Abgang zur Verfügung gestellt werden.   07790005
      *    Beide Felder sind als  PIC S9(014)V9  PACKED-DECIMAL         07791005
      *    definiert.                                                   07792005
      *                                                                 07800005
      *    Der ermittelte EQF wird in Z-DWVB0001-EQF in der Form        07810005
      *    PIC S9(003)V9 PACKED-DECIMAL bereitgestellt.                 07840009
      *                                                                 07840110
      *    Der ermittelte EQF wird                                      07840210
      *    in Z-DWVB0001-EQF-NR mit PIC S9(003)V99 ungerundet und       07840410
      *    in Z-DWVB0001-EQF    mit PIC S9(003)V9  gerundet             07840510
      *    und PACKED DECIMAL bereitgestellt.                           07840610
      *                                                                 07841005
      * Berechnungsgrundlage:                                           07851005
      *                                                                 07852005
      *    Falls der Abgang negativ und der Zugang positiv              07853005
      *    (größer Null) ist, wird der EQF berechnet; dies geschieht    07854009
      *    mittels Korrektur des Vorzeichens von Abgang.                07855009
      *    Falls der Abgang nun grösser ist als der Zugang, wird        07856109
      *    der EQF auf 100 gesetzt; andernfalls wird der Abgang         07856209
      *    mit 100 multipliziert und durch den Zugang dividiert.        07857009
      *                                                                 07859205
      *    Falls der Abgang negativ und der Zugang negativ              07859305
      *    oder Null ist, wird 100 in EQF gesetzt.                      07859405
      *                                                                 07890005
      *    Falls der Abgang positiv oder Null ist und der               07900005
      *    Zugang negativ ist, wird der EQF durch Addition              07910005
      *    von Abgang und Zugang ermittelt. Ist das Ergebnis            07920005
      *    negativ, wird 100, anderenfalls wird ZERO in EQF             07930005
      *    eingetragen.                                                 07940005
281194*    (entfällt; 28.11.1994)                                       07960005
      *                                                                 07950005
      *    Falls der Abgang positiv oder Null ist und der               07960005
      *    Zugang positiv oder Null ist, wird ZERO in EQF               07970005
      *    gestellt.                                                    07971005
281194*    (entfällt; 28.11.1994)                                       07960005
      *                                                                 07950005
281194*    Falls der Abgang positiv oder Null ist, wird Zero            07960005
281194*    in den EQF eingestellt.                                      07970005
      *                                                                 07980005
                                                                        08390104
                                                                        08392004
           IF Z-DWVB0001-ABGANG < ZERO                                  08392104
              IF Z-DWVB0001-ZUGANG > ZERO                               08392204
                 COMPUTE Z-DWVB0001-ABGANG = Z-DWVB0001-ABGANG * -1     08392607
                 IF Z-DWVB0001-ABGANG  > Z-DWVB0001-ZUGANG              08392707
                    MOVE 100 TO Z-DWVB0001-EQF-NR                       08392810
                 ELSE                                                   08392907
                    COMPUTE Z-DWVB0001-EQF-NR =                         08393010
                         Z-DWVB0001-ABGANG * 100 / Z-DWVB0001-ZUGANG    08393107
                 END-IF                                                 08393204
              ELSE                                                      08393304
                 MOVE 100 TO Z-DWVB0001-EQF-NR                          08393410
              END-IF                                                    08393504
           ELSE                                                         08393604
281194*       IF Z-DWVB0001-ZUGANG < ZERO                               08393704
281194*       AND (Z-DWVB0001-ABGANG + Z-DWVB0001-ZUGANG ) < 0          08393804
281194*           MOVE 100 TO Z-DWVB0001-EQF-NR                         08393910
281194*       ELSE                                                      08394005
281194*           MOVE ZERO TO Z-DWVB0001-EQF-NR                        08394110
281194*       END-IF                                                    08394204
281194        MOVE ZERO TO Z-DWVB0001-EQF-NR                            08394110
           END-IF                                                       08395004
           COMPUTE Z-DWVB0001-EQF ROUNDED = Z-DWVB0001-EQF-NR * 1       08396010
           .                                                            08400000
       EQF-BERECHNEN-EXIT.                                              08401006
           EXIT.                                                        08410006
