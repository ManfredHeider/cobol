      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0361.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0361.CPv  $
      *
      *   Rev 2.4   Feb 11 2020 12:03:12   t13ae34
      *FAN01178: eAntrag Geiger Direktkauf- Telefonnummer ist im Di
      *alog nicht aufgeführt!
      *
      *   Rev 2.3   Nov 25 2019 11:21:50   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.2   Nov 21 2019 13:41:08   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.0   Oct 22 2019 11:56:12   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0361                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0361                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.11.2019 / 13:17:51 / T13AE34
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01396!21.11.19!    ! NEU                       ! HEIDER   *
      * FAN01178!10.02.20!    ! GEWS_KUNDE_EMAIL _TELEFON ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0361
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0361.
           05  PI-DRAN0361-KEY.

             10 PI-DRAN0361-GEWS-PIN   PIC S9(9) COMP.

             10 PI-DRAN0361-GEWS-AVERM   PIC S9(9) COMP.
             10 PI-DRAN0361-GEWS-AVERM-NAME   PIC  X(0060).
             10 PI-DRAN0361-GEWS-BANK-TYP   PIC  X(0005).
             10 PI-DRAN0361-GEWS-BETRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRAN0361-GEWS-EINMALBETRAG   PIC S9(8)V9(2) COMP-3.
100220       10 PI-DRAN0361-GEWS-KUNDE-EMAIL  PIC  X(050).
             10 PI-DRAN0361-GEWS-ENDE-DATUM   PIC  X(010).
             10 PI-DRAN0361-GEWS-GEBURT-DATUM   PIC  X(010).
             10 PI-DRAN0361-GEWS-GGSP   PIC  X(0004).
             10 PI-DRAN0361-GEWS-IBAN   PIC  X(0034).
             10 PI-DRAN0361-GEWS-INNENDIENST   PIC  X(0001).
             10 PI-DRAN0361-GEWS-KENNUNG-INS   PIC  X(0008).
             10 PI-DRAN0361-GEWS-KONTOINHABER   PIC  X(0060).
             10 PI-DRAN0361-GEWS-KUNDE-ANREDE   PIC  X(0005).
             10 PI-DRAN0361-GEWS-KUNDE-NAME   PIC  X(0026).
             10 PI-DRAN0361-GEWS-KUNDE-TITEL   PIC  X(0020).
             10 PI-DRAN0361-GEWS-KUNDE-VORNAME   PIC  X(0026).
             10 PI-DRAN0361-GEWS-LAND   PIC  X(0030).
             10 PI-DRAN0361-GEWS-MANDANT   PIC  X(0010).
             10 PI-DRAN0361-GEWS-OKI-DATUM   PIC  X(0026).
             10 PI-DRAN0361-GEWS-ORT   PIC  X(0030).
             10 PI-DRAN0361-GEWS-PLZ   PIC  X(0006).
             10 PI-DRAN0361-GEWS-START-DATUM   PIC  X(010).
             10 PI-DRAN0361-GEWS-STRASSE   PIC  X(0045).
100220       10 PI-DRAN0361-GEWS-KUNDE-TELEFON   PIC  X(0030).
             10 PI-DRAN0361-GEWS-VM   PIC S9(9) COMP.
             10 PI-DRAN0361-GEWS-VM-NAME   PIC  X(0060).
             10 PI-DRAN0361-GEWS-ZAHLWEISE   PIC S9(4) COMP.

      *
       01  PO-DRAN0361.
           05  PO-DRAN0361-RC                     PIC  X(003).
               88 PO-DRAN0361-RC-OK               VALUE 'OK '.
               88 PO-DRAN0361-RC-DUPLICATE-VALUE  VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************



