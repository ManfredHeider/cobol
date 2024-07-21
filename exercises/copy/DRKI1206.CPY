      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1206.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1206.CPv  $
      *
      *   Rev 2.1   02 Apr 2001 10:41:56   T13EX58
      *AKI00169
      *Standardanpassung für CKV
      *
      *   Rev 2.0   12 Mar 2001 08:55:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1206                                     *
      *                                                              *
      *    FUNKTION: VSA-RELEVANTE DATEN AUFLISTEN                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: W. MORFELD                                      *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00169!15.03.01! 01 !NEU                        ! WMO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0964                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: VSA-RELEVANTE DATEN AUFLISTEN                   *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1206.
          05  PI-DRKI1206-PROGRAMM                PIC X(008).
          05  PI-DRKI1206-UFKT                    PIC X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05  PI-DRKI1206-DATEN.
              07 PI-DRKI1206-KVGESCHAEFTSTID      PIC S9(04) COMP.
              07 PI-DRKI1206-KVGESCHAEFTSTTEXT    PIC X(30).

      ***
       01 PO-DRKI1206.
          05 PO-DRKI1206-RC                       PIC X(003).
             88 PO-DRKI1206-RC-OK                     VALUE 'OK '.
             88 PO-DRKI1206-RC-LEER                   VALUE 'LER'.
             88 PO-DRKI1206-RC-EOF                    VALUE 'EOF'.
          05 PO-DRKI1206-DATEN.
             10 PO-DRKI1206-KVGESCHAEFTSTID       PIC S9(04) COMP.
      ******** Ende Copy-Member **************************************
