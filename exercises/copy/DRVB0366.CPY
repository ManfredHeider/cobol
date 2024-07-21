      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0366.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0366.CPv  $
      *
      *   Rev 2.0   Apr 11 2000 10:50:42   T13AE30
      *ACV00013 - Neuer VB-Vertrag: Bestätigunggsdruck
      *
      *   Rev 1.0   22 Jan 1999 13:41:04   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0366                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0073                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    03.04.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00013!03.04.00! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0366                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0366.
          05 PI-DRVB0366-PROGRAMM           PIC  X(008).
          05 PI-DRVB0366-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0366-ANSCHREIB-TYP      PIC S9(004)  COMP.
      *
       01 PO-DRVB0366.

          05 PO-DRVB0366-RC                 PIC  X(003).
             88 PO-DRVB0366-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0366-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0366-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0366-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0366-DATEN.
             10 PO-DRVB0366-BA-TUPEL-GESAMT      PIC X(118).

      *
      ***** Ende Copy-Member DRVB0366 *****************************
