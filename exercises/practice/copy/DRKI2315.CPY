      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2315.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2315.CPv  $
      *
      *   Rev 2.3   Mar 27 2017 09:28:52   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.2   Aug 30 2011 12:50:42   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.1   May 22 2009 17:36:58   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 20 2009 11:53:50   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      **************************************************************** *
      *                                                                *
      *           Mandantenwechsel ohne KSDS-Verschiebelogik           *
      *                 Copystrecke fuer Ausgabedatei                  *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:    80 BYTES                 STAND: 20.03.2009      *
      *                                                                *
      **************************************************************** *
       01 DRKI2315-MANDANTENWECHSEL.
      *
          05 PARTGESELLSCHID      PIC 9(4).
      *       Gesellschafts-Nr.   z. Bsp. 10 = AM
      *                                   66 = Santander
      *       siehe Tabelle TCKI0332
          05 VERTRAGSNUMMER       PIC X(15).
      *
          05 MANDANTNR-ALT        PIC 9(9).
      *      004400001 = DVAG
      *      004400003 = ALLFINANZ
          05 MANDANTNR-NEU        PIC 9(9).
      *      004400001 = DVAG
      *      004400003 = ALLFINANZ
          05 ABSCHLVBNR-ALT       PIC 9(9).
      *      DVAG-ABSCHLUSSVERMITTLER
          05 ABSCHLVBNR-NEU       PIC 9(9).
      *      DVAG-ABSCHLUSSVERMITTLER
          05 MANDWECHSEL-FILLER   PIC X(25).
      *
      *
      ******************************************************************
      *                 E N D E    COPY-MEMBER DRKI2315                *
      ******************************************************************

