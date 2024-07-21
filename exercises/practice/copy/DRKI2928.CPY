      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2928.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2928.CPv  $
      *
      *   Rev 2.2   Mar 02 2015 16:24:02   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.1   Sep 01 2014 10:36:02   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.0   Jul 16 2014 18:09:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2928                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2928                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01315!16.07.14!2.1 ! neu                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2928
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2928.
          05 PI-DRKI2928-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2928-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2928-KEY.
            10 PI-DRKI2928-FOPPALETTENNAME        PIC  X(008).
 
      *
       01  PO-DRKI2928.
           05  PO-DRKI2928-RC                     PIC  X(003).
             88 PO-DRKI2928-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2928-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2928-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2928-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2928-DATEN.
 
           10 PO-DRKI2928-FOPPALETTENNAME PIC  X(008) OCCURS 100.
           10 PO-DRKI2928-FOPFONDSKUERZEL PIC  X(002) OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************

