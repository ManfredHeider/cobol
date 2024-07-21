      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0553.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0553.CPv  $
      *
      *   Rev 2.1   09 Jul 2001 12:24:42   t13ex57
      *AVB00551 - Ermitteln aktueller sowie historischer  
      *PINs (VB Portal)
      *
      *   Rev 2.0   15 May 2001 13:33:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0553                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0081                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: UTE BOLL                                         *
      *    DATUM:    04.07.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00551!04.07.01! 01 ! NEU                       !UTE BOLL  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0553                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0553.
          05 PI-DRVB0553-UFKT-CODE             PIC  9(002).
          05 PI-DRVB0553-ST-NR                 PIC S9(009)  COMP.

      *
       01 PO-DRVB0553.
          05 PO-DRVB0553-RC                    PIC  X(003).
             88 PO-DRVB0553-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0553-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0553-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0553-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0553-DATEN.
             10 PO-DRVB0553-MA-ST-NR           PIC S9(9)  COMP.
             10 PO-DRVB0553-MPW                PIC X(10).
             10 PO-DRVB0553-MMPZ-DAT-VON       PIC X(10).
             10 PO-DRVB0553-MMPZ-LAST-UPDATE   PIC X(26).
             10 PO-DRVB0553-MMPZ-DAT-BIS       PIC X(10).

      *
      ***** ENDE COPY-MEMBER DRVB0553 *****************************

