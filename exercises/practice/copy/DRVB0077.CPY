      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0077.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0077.CPv  $
      *
      *   Rev 2.1   16 Nov 2007 11:54:58   t13ex43
      *AVB00945: PGM - IT-interne Anpassungen für Allfinanz (Dialog
      *-Infozeile)
      *
      *   Rev 2.0   Feb 27 1999 15:57:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:29:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0077                                      *
      *    --------------------------------------------------------   *
      *    DEFINITION DER UNTERSTUFEN DER INFOZEILE FUER DRVB0077     *
      *    --------------------------------------------------------   *
      *    ==> DRVB0001,                                              *
      *        DRVB0003 BEACHTEN BEI ÄNDERUNGEN!!                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GERLACH                                          *
      *****************************************************************
      *    VERSION:  1    10.03.95 16:02:17
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG !  DATUM ! VER! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      *         !10.02.95! 01 ! NEU                       ! GERLACH  *
261007* AVB00945!26.10.07! 2.1! GESELL HINZU, LÄNGE BLEIBT! KLEEMEIER*
      *                                                              *
      ****************************************************************
      *
      *
           15 FILLER                    PIC X(04)    VALUE 'TYP:'.
           15 Z-DRVB0077-MA-TYP-BEZ     PIC X(02).
           15 FILLER                    PIC X(07)    VALUE ' STUFE:'.
           15 Z-DRVB0077-MS-BEZ-KURZ.
              20 Z-DRVB0077-MS-BEZ      PIC X(04).
              20 Z-DRVB0077-MS-VM-INAKT PIC X(02).
           15 FILLER                    PIC X(01)    VALUE SPACE.
261007**** 15 FILLER                    PIC X(05)    VALUE ' VBP:'.
261007     15 FILLER                    PIC X(03)    VALUE ' P:'.
           15 Z-DRVB0077-VBP-NR         PIC Z.
           15 FILLER                    PIC X(06)    VALUE ' BETR:'.
           15 Z-DRVB0077-BETR-ST-NR     PIC X(07).
           15 FILLER                    PIC X(01)    VALUE SPACE.
           15 Z-DRVB0077-BETR-ST-PRZ    PIC X(01).
           15 FILLER                    PIC X(05)    VALUE ' DIR:'.
           15 Z-DRVB0077-DIR-NR         PIC X(03).
261007     15 FILLER                    PIC X(01)    VALUE SPACE.
261007     15 Z-DRVB0077-GESELL         PIC X(04).
261007**** 15 FILLER                    PIC X(06)    VALUE '    A:'.
261007     15 FILLER                    PIC X(03)    VALUE ' A:'.
           15 Z-DRVB0077-MAT-DAT-BIS    PIC X(10).
      *
      *
      ******** ENDE COPY-MEMBER (DRVB0077) *******************
