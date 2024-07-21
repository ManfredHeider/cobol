      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0342.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0342.CPv  $
      *
      *   Rev 2.0   Apr 20 1999 16:34:30   T13EX31
      *AVB00396  neu
      *
      *
      *   Rev 1.0   27 Oct 1998 16:56:22   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0342                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0031                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    05.03.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!05.03.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0342                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0342.
          05 PI-DRVB0342-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0342-PCZ-BEZUGS-NR      PIC S9(009) COMP.
      *
       01 PO-DRVB0342.

          05 PO-DRVB0342-RC                 PIC  X(003).
             88 PO-DRVB0342-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0342-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0342-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0342-RC-NOT-FOUND                 VALUE 'NFD'.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0031                           *
      ******************************************************************
        05 PO-DRVB0342-DATEN.
          10 WB2-NI-VIVB0031-TAB.
             15 WB2-NI-VIVB0031-ELE PIC S9(04)  COMP OCCURS  9  TIMES.
          10 WB2-NI-VIVB0031        REDEFINES   WB2-NI-VIVB0031-TAB.
             15 WB2-NI-PCZ-BEZUGS-NR   PIC S9(04)  COMP.
             15 WB2-NI-PCZ-NR          PIC S9(04)  COMP.
             15 WB2-NI-PCZ-DAT-VON     PIC S9(04)  COMP.
             15 WB2-NI-PCZ-LAST-UPDATE PIC S9(04)  COMP.
             15 WB2-NI-KTY-BEZUGS-TYP  PIC S9(04)  COMP.
             15 WB2-NI-KNR-BEZUGS-TYP  PIC S9(04)  COMP.
             15 WB2-NI-KTY-PCZ-TYP     PIC S9(04)  COMP.
             15 WB2-NI-KNR-PCZ-TYP     PIC S9(04)  COMP.
             15 WB2-NI-PCZ-KENNZEICHEN PIC S9(04)  COMP.
          10  WB2-VIVB0031.
             15 WB2-PCZ-BEZUGS-NR      PIC S9(9)   COMP.
             15 WB2-PCZ-NR             PIC S9(9)   COMP.
             15 WB2-PCZ-DAT-VON        PIC X(10).
             15 WB2-PCZ-LAST-UPDATE    PIC X(26).
             15 WB2-KTY-BEZUGS-TYP     PIC S9(4)   COMP.
             15 WB2-KNR-BEZUGS-TYP     PIC S9(4)   COMP.
             15 WB2-KTY-PCZ-TYP        PIC S9(4)   COMP.
             15 WB2-KNR-PCZ-TYP        PIC S9(4)   COMP.
             15 WB2-PCZ-KENNZEICHEN    PIC X(1).
      ***** ENDE COPY-MEMBER DRVB0196 *****************************
