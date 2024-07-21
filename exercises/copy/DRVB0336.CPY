      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0336.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0336.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   18 Nov 1998 12:17:02   T13AE42
      *AAS00078 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   27 Oct 1998 16:35:06   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0336                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0007                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT,B.                                       *
      *    DATUM:    02.11.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00024!02.11.98! 01 ! NEUERSTELLUNG             ! SCHMIDT,B*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0336                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0336.
          05 PI-DRVB0336-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0336-VBPZ-DAT-VON       PIC X(010).
          05 PI-DRVB0336-VBPZ-DAT-BIS       PIC X(010).
      *
       01 PO-DRVB0336.

          05 PO-DRVB0336-RC                 PIC  X(003).
             88 PO-DRVB0336-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0336-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0336-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0336-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0336-DATEN.
             10 PO-DRVB0336-VBP-NR           PIC S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0336 *****************************
