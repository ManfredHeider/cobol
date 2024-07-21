      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1900.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1900.CPv  $
      *
      *   Rev 2.4   Feb 20 2019 13:13:28   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gem‰ﬂ VB-Vertrag
      *
      *   Rev 2.3   Feb 20 2019 09:11:40   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gem‰ﬂ VB-Vertrag
      *
      *   Rev 2.2   06 Feb 2019 15:20:54   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gem‰ﬂ VB-Vertrag
      *
      *   Rev 2.1   Dec 11 2018 13:18:26   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gem‰ﬂ VB-Vertrag
      *
      *   Rev 2.0   Jul 24 2018 13:53:38   T13EX44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1900 FUNKTION DES UPPA0142                *
      *    -------------------------------------------------------    *
      *    ERMITTLUNG ALLER FAKTOREN FUER DIE AUSGLEICHSANSPRUECHE    *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA02659!24.07.18!2.0 ! NEU                       ! SCHEIDEL  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHƒLT DIE EIN- UND AUSGABEPARAMETER          *
      *    F‹R DIE FUNKTION DRPA1900                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRPA1900.
          05 PI-DRPA1900-DATEN.
             10 PI-DRPA1900-VBNR       PIC S9(009)       BINARY.
             10 PI-DRPA1900-EDNR-TAET-DAT
                                       PIC S9(009)       BINARY.
             10 PI-DRPA1900-ABR-DAT-BIS
                                       PIC  X(010).
      *
       01 PO-DRPA1900.
          05 PO-DRPA1900-RC            PIC  X(003).
             88 PO-DRPA1900-RC-OK           VALUE 'OK '.
             88 PO-DRPA1900-RC-NOT-RELEV    VALUE 'NRV'.
          05 PO-DRPA1900-RD-NRV-TEXT   PIC  X(030).
          05 PO-DRPA1900-DATEN.
      *
      * INFORMATIONEN ZUM VB SELBST
             10 PO-DRPA1900-MAN-TITEL-ADEL
                                       PIC  X(010).
             10 PO-DRPA1900-MAN-TITEL-AKAD
                                       PIC  X(010).
             10 PO-DRPA1900-MAN-VORNAME
                                       PIC  X(020).
             10 PO-DRPA1900-MAN-NAME   PIC  X(040).
      *
      * ALLGEMEINE GESELLSCHAFTSFAKTOREN
             10 PO-DRPA1900-GES-FAK-G10-S
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-GES-FAK-G10-K
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-GES-FAK-G20
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-GES-FAK-G50
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-GES-FAK-G70
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-GES-FAK-G81
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-ED     PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-TAETIG
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
      *
      * FUER GES 70 WERDEN NUR VIER JAHRE BERUECKSICHTIGT. DAHER GIBT
      * ES FUER DIESE GESELLSCHAFT EIGENE FAKTOREN.
             10 PO-DRPA1900-FAK-ED-G70 PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-TAETIG-G70
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
      *
      * FAKTORENDEFINITIONEN
      * -T: TAETIGKEITSFAKTOR DES VBS FUER DIE GESELLSCHAFT
      * -B: TREUEBONUSFAKTOR DES VBS (NUR FUER GES 70)
             10 PO-DRPA1900-FAK-G10-S-T
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-G10-K-T
                                       PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-G20-T  PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-G50-T  PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-G70-T  PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-G70-B  PIC S9(001)V9(08) PACKED-DECIMAL.
             10 PO-DRPA1900-FAK-G81-T  PIC S9(001)V9(08) PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 
 

