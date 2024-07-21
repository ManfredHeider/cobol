      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1119.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1119.CPv  $
      *
      *   Rev 2.2   Jan 05 2016 14:05:50   t13ae30
      *AVB01379: Neue Funktion ZWVB0001
      *
      *   Rev 2.1   Oct 06 2015 11:12:22   t13ae14
      *AVB01379: Neue Funktion ZWVB0001
      *
      *   Rev 2.0   Oct 05 2015 15:04:30   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1119                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1119                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.Ober                                           *
      *    DATUM:    05.10.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01379!05.10.15! 2.1! NEU                       ! K.Ober   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1119: LESEN ST-NR
      *                               ERM. OB VBNR VERGEBEN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01 PI-DRVB1119.
          05 PI-DRVB1119-DATEN.
             10 PI-DRVB1119-ST-NR            PIC S9(009).
      *
       01 PO-DRVB1119.
          05 PO-DRVB1119-RC                  PIC  X(003).
             88 PO-DRVB1119-RC-OK            VALUE 'OK '.
             88 PO-DRVB1119-RC-NFD           VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************


