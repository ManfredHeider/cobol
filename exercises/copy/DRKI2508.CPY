      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2508.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2508.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 16:16:18   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Adressen
      *
      *   Rev 2.0   Jun 20 2011 15:25:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2508                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2508                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.06.2011 / 15:34:01 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2508
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2508.
           05  PI-DRKI2508-KEY.
             10 PI-DRKI2508-ADRESSELANDID          PIC X(003).
             10 PI-DRKI2508-ADRESSEPLZ             PIC X(015).
             10 PI-DRKI2508-ADRESSEORT             PIC X(040).
             10 PI-DRKI2508-ADRESSESUCHSTRASSE     PIC X(045).

      *
       01  PO-DRKI2508.
           05  PO-DRKI2508-RC                     PIC  X(003).
               88 PO-DRKI2508-RC-OK               VALUE 'OK '.
               88 PO-DRKI2508-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2508-DATEN.
             10 PO-DRKI2508-QUALITAET.
               49 PO-DRKI2508-QUALITAET-LEN        PIC S9(004) COMP.
               49 PO-DRKI2508-QUALITAET-TEXT       PIC  X(001).
             10 PO-DRKI2508-ADRESSEID              PIC  X(013).
      *
      ******* ENDE COPY-MEMBER ***************************************
