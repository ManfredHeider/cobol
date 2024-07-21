      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0364.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0364.CPv  $
      *
      *   Rev 2.4   Feb 11 2020 12:04:08   t13ae34
      *FAN01178: eAntrag Geiger Direktkauf- Telefonnummer ist im Di
      *alog nicht aufgeführt!
      *
      *   Rev 2.3   Nov 25 2019 11:23:10   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.2   Nov 21 2019 13:41:50   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.0   Nov 21 2019 13:17:56 T13AE34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0364                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0364                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.11.2019 / 13:17:56 / T13AE34
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01396!21.11.19!    ! NEU                       ! HEIDER   *
      * FAN01178!10.02.20!    ! TELEFON EMAIL             ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0364
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0364.
           05  PI-DRAN0364-KEY.

             10 PI-DRAN0364-GEWS-PIN   PIC S9(9) COMP.
      *
       01  PO-DRAN0364.
           05  PO-DRAN0364-RC                     PIC  X(003).
               88 PO-DRAN0364-RC-OK               VALUE 'OK '.
               88 PO-DRAN0364-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRAN0364-DATEN.

             10 PO-DRAN0364-GEWS-AVERM   PIC S9(9) COMP.
             10 PO-DRAN0364-GEWS-AVERM-NAME   PIC  X(0060).
             10 PO-DRAN0364-GEWS-BANK-TYP   PIC  X(0005).
             10 PO-DRAN0364-GEWS-BETRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRAN0364-GEWS-EINMALBETRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRAN0364-GEWS-KUNDE-EMAIL  PIC  X(050).
             10 PO-DRAN0364-GEWS-ENDE-DATUM   PIC  X(010).
             10 PO-DRAN0364-GEWS-GEBURT-DATUM   PIC  X(010).
             10 PO-DRAN0364-GEWS-GGSP   PIC  X(0004).
             10 PO-DRAN0364-GEWS-IBAN   PIC  X(0034).
             10 PO-DRAN0364-GEWS-INNENDIENST   PIC  X(0001).
             10 PO-DRAN0364-GEWS-KENNUNG-INS   PIC  X(0008).
             10 PO-DRAN0364-GEWS-KENNUNG-UPD   PIC  X(0008).
             10 PO-DRAN0364-GEWS-KONTOINHABER   PIC  X(0060).
             10 PO-DRAN0364-GEWS-KUNDE-ANREDE   PIC  X(0005).
             10 PO-DRAN0364-GEWS-KUNDE-NAME   PIC  X(0026).
             10 PO-DRAN0364-GEWS-KUNDE-TELEFON   PIC  X(0030).
             10 PO-DRAN0364-GEWS-KUNDE-TITEL   PIC  X(0020).
             10 PO-DRAN0364-GEWS-KUNDE-VORNAME   PIC  X(0026).
             10 PO-DRAN0364-GEWS-LAND   PIC  X(0030).
             10 PO-DRAN0364-GEWS-MANDANT   PIC  X(0010).
             10 PO-DRAN0364-GEWS-OKI-DATUM   PIC  X(0026).
             10 PO-DRAN0364-GEWS-ORT   PIC  X(0030).
             10 PO-DRAN0364-GEWS-PLZ   PIC  X(0006).
             10 PO-DRAN0364-GEWS-START-DATUM   PIC  X(010).
             10 PO-DRAN0364-GEWS-STRASSE   PIC  X(0045).
             10 PO-DRAN0364-GEWS-VM   PIC S9(9) COMP.
             10 PO-DRAN0364-GEWS-VM-NAME   PIC  X(0060).
             10 PO-DRAN0364-GEWS-ZAHLWEISE   PIC S9(4) COMP.
             10 PO-DRAN0364-GEWS-ZEIT-INS   PIC  X(0026).
             10 PO-DRAN0364-GEWS-ZEIT-UPD   PIC  X(0026).

      *
      ******* ENDE COPY-MEMBER ***************************************



