      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1304.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1304.CPv  $
      *
      *   Rev 2.1   Jul 05 2004 10:33:02   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Apr 23 2002 16:01:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1304                                     *
      *                                                              *
      *    FUNKTION: PI/PO                                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: W. HORNUNG                                      *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00082!11.06.04! 01 !NEU                        ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1304                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: VSA-RELEVANTE DATEN AUFLISTEN                   *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1304.
          05  PI-DRKI1304-PROGRAMM                PIC X(008).
          05  PI-DRKI1304-UFKT                    PIC X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05  PI-DRKI1304-DATEN.
              07 PI-DRKI1304-VERTRAGSNUMMER       PIC X(13).

      ***
       01 PO-DRKI1304.
          05 PO-DRKI1304-RC                       PIC X(003).
             88 PO-DRKI1304-RC-OK                     VALUE 'OK '.
             88 PO-DRKI1304-RC-LEER                   VALUE 'LER'.
             88 PO-DRKI1304-RC-EOF                    VALUE 'EOF'.
          05 PO-DRKI1304-DATEN.
             10 PO-DRKI1304-ARBEITSAUFTRAGID            PIC X(13) .
      ******** Ende Copy-Member **************************************
