      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1653.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1653.CPv  $
      *
      *   Rev 2.1   Oct 24 2005 10:25:54   t13ex25
      *APA00385: Vermögenssicherungspolice AM-Vers
      *
      *   Rev 2.0   Oct 12 2005 09:30:10   t13ex25
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1653  EVS-Struktur anpassen              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    08.07.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00385!11.10.05! 02 ! NEUERSTELLUNG             ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1653                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1653.
          05 PI-DRPA1653-PZVP-VERSION           PIC S9(004) BINARY.
          05 PI-DRPA1653-PROV-TERMIN            PIC  X(010).

       01 PO-DRPA1653.
          05 PO-DRPA1653-RC                     PIC  X(003).
             88 PO-DRPA1653-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1653-RC-VSTAT-GEFUELLT        VALUE 'VSG'.
          05 PO-DRPA1653-DATEN.
             10 PO-DRPA1653-STRK-KZ             PIC  X(001).
      *
      ******** ENDE COPY-MEMBER DRPA1653 *****************************
