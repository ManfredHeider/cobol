      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1417.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1417.CPv  $
      *
      *   Rev 2.1   Jul 30 2002 16:56:28   t13ex39
      *6524284
      *
      *   Rev 2.0   Apr 23 2002 16:01:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1417                                     *
      *                                                              *
      *    FUNKTION: C_CONTROL-RELEVANTE DATEN AUFLISTEN             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: W. HORNUNG                                      *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!10.05.02! 01 !NEU                        ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1417                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: VSA-RELEVANTE DATEN AUFLISTEN                   *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1417.
          05  PI-DRKI1417-PROGRAMM                PIC X(008).
          05  PI-DRKI1417-UFKT                    PIC X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05  PI-DRKI1417-DATEN.
              07 PI-DRKI1417-CON-OBJECT-ID        PIC X(13).

      ***
       01 PO-DRKI1417.
          05 PO-DRKI1417-RC                       PIC X(003).
             88 PO-DRKI1417-RC-OK                     VALUE 'OK '.
             88 PO-DRKI1417-RC-LEER                   VALUE 'LER'.
             88 PO-DRKI1417-RC-EOF                    VALUE 'EOF'.
          05 PO-DRKI1417-DATEN.
             10 PO-DRKI1417-CON-ID                      PIC S9(9) COMP.
             10 PO-DRKI1417-CON-TIMESTAMP               PIC X(26).
             10 PO-DRKI1417-CON-SUBTYPE                 PIC S9(4) COMP.
             10 PO-DRKI1417-CON-REASON                  PIC X(60).
             10 PO-DRKI1417-CON-USER                    PIC X(30).
             10 PO-DRKI1417-CON-OBJ-CLASS-ID            PIC S9(9) COMP.
             10 PO-DRKI1417-CON-CC-REG-ID               PIC S9(9) COMP.
      ******** Ende Copy-Member **************************************
