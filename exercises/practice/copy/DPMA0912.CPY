      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPMA0912.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPMA0912.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 14:58:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    Das nachfolgende Copy-Member ist von der DVAG übernommen   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Wolff                                         *
      *    DATUM:    05.12.1994                                       *
      *****************************************************************
      *
      *
      *****************************************************************
      *    COPY-MEMBER: DPVB0001       EQF-Berechung                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. KUHN / A.Priggen                              *
      *    DATUM:    26.10.1994                                       *
      *    STAND:    26.10.1994                                       *
      *****************************************************************
      *    VERSION   1    28.10.94 10:11:38
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !26.10.94! 01 ! NEU                       ! Priggen  *
ARS*  *         !28.11.94! 02 ! Korrektur EQF-Berechnung  ! Priggen  *
ARS*  *         !        !    ! gem. Memo v. 28.11.94     !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *---------------------------------------------------------------
       EQF-BERECHNEN SECTION.
      *---------------------------------------------------------------
      * Übergabe vor Aufruf:
      *
      *    Einheiten müssen als Zugang in Z-DWVB0001-Zugang und als
      *    Abgang in Z-DWVB0001-Abgang zur Verfügung gestellt werden.
      *    Beide Felder sind als  PIC S9(014)V9  PACKED-DECIMAL
      *    definiert.
      *
      *    Der ermittelte EQF wird in Z-DWVB0001-EQF in der Form
      *    PIC S9(003)V9 PACKED-DECIMAL bereitgestellt.
      *
      *    Der ermittelte EQF wird
      *    in Z-DWVB0001-EQF-NR mit PIC S9(003)V99 ungerundet und
      *    in Z-DWVB0001-EQF    mit PIC S9(003)V9  gerundet
      *    und PACKED DECIMAL bereitgestellt.
      *
      * Berechnungsgrundlage:
      *
      *    Falls der Abgang negativ und der Zugang positiv
      *    (größer Null) ist, wird der EQF berechnet; dies geschieht
      *    mittels Korrektur des Vorzeichens von Abgang.
      *    Falls der Abgang nun grösser ist als der Zugang, wird
      *    der EQF auf 100 gesetzt; andernfalls wird der Abgang
      *    mit 100 multipliziert und durch den Zugang dividiert.
      *
      *    Falls der Abgang negativ und der Zugang negativ
      *    oder Null ist, wird 100 in EQF gesetzt.
      *
      *    Falls der Abgang positiv oder Null ist und der
      *    Zugang negativ ist, wird der EQF durch Addition
      *    von Abgang und Zugang ermittelt. Ist das Ergebnis
      *    negativ, wird 100, anderenfalls wird ZERO in EQF
      *    eingetragen.
ARS*  *    (entfällt; 28.11.1994)
      *
      *    Falls der Abgang positiv oder Null ist und der
      *    Zugang positiv oder Null ist, wird ZERO in EQF
      *    gestellt.
ARS*  *    (entfällt; 28.11.1994)
      *
ARS*  *    Falls der Abgang positiv oder Null ist, wird Zero
ARS*  *    in den EQF eingestellt.
      *


           IF Z-DWVB0001-ABGANG < ZERO
              IF Z-DWVB0001-ZUGANG > ZERO
                 COMPUTE Z-DWVB0001-ABGANG = Z-DWVB0001-ABGANG * -1
                 IF Z-DWVB0001-ABGANG  > Z-DWVB0001-ZUGANG
                    MOVE 100 TO Z-DWVB0001-EQF-NR
                 ELSE
                    COMPUTE Z-DWVB0001-EQF-NR =
                         Z-DWVB0001-ABGANG * 100 / Z-DWVB0001-ZUGANG
                 END-IF
              ELSE
                 MOVE 100 TO Z-DWVB0001-EQF-NR
              END-IF
           ELSE
ARS   *       IF Z-DWVB0001-ZUGANG < ZERO
ARS   *       AND (Z-DWVB0001-ABGANG + Z-DWVB0001-ZUGANG ) < 0
ARS   *           MOVE 100 TO Z-DWVB0001-EQF-NR
ARS   *       ELSE
ARS   *           MOVE ZERO TO Z-DWVB0001-EQF-NR
ARS   *       END-IF
ARS           MOVE ZERO TO Z-DWVB0001-EQF-NR
           END-IF
           COMPUTE Z-DWVB0001-EQF ROUNDED = Z-DWVB0001-EQF-NR * 1
           .
       EQF-BERECHNEN-EXIT.
           EXIT.
