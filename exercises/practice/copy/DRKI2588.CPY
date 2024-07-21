      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2588.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2588.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:59:46   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Mar 29 2012 17:33:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2588                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2588                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!14.12.11!2.1 ! neu                       !I. Frenzel*
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2588
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI2588.
          05 PI-DRKI2588-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2588-UFKT-CODE                PIC  9(002).
      *
       01 PO-DRKI2588.

          05 PO-DRKI2588-RC                       PIC  X(003).
             88 PO-DRKI2588-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2588-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2588-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI2588-DATEN.
            10 PO-DRKI2588-KNDPERSONENID         PIC X(013).
            10 PO-DRKI2588-KNDANZNAME            PIC X(030).
            10 PO-DRKI2588-KNDANZVORNAME         PIC X(030).
      *
      ********** Ende Copy-Member DRKI2588 ****************************
