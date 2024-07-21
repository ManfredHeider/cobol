      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1133.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1133.CPv  $
      *
      *   Rev 2.1   31 Jan 2001 14:25:36   T13EX39
      *AKI00068 KBI
      *
      *   Rev 2.0   07 Jun 2000 17:08:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1133 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    14.02.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068!14.02.00! 01 !NEU                        ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1133                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *

      ****************************************************************
       01 PI-DRKI1133.
           05  PI-DRKI1133-PROGRAMM             PIC  X(008).
           05  PI-DRKI1133-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1133-DATEN.
               10 PI-DRKI1133-VEGVERTRAGSID     PIC X(13).
               10 PI-DRKI1133-VEGVERSTARLFDNR   PIC S9(4) COMP.
      ***
       01 PO-DRKI1133.
          05 PO-DRKI1133-RC                     PIC  X(003).
             88 PO-DRKI1133-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1133-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1133-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1133-DATEN.
             10 PO-DRKI1133-VEGGEFAHRCODE       PIC S9(4) COMP.
      ******** Ende Copy-Member **************************************

