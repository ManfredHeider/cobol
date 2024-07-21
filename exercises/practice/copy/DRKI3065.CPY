      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3065.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3065.CPv  $
      *
      *   Rev 2.1   Apr 08 2016 09:00:32   t13ex73
      *AKI01425: G70 Gültigkeitsdatum der NV-Bescheinigung - PGMs
      *
      *   Rev 2.0   Mar 17 2016 11:48:08   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 3 0 6 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G70 Aktualisierung der FSA-Date              *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  169 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *AUFTRAG !DATUM   !AENDERUNG                           !NAME     *
      *        !        !                                    !         *
      *--------!--------!------------------------------------!---------*
      *AKI01425!17.03.16! NEUERSTELLUNG                      !DDY      *
      *        !        !                                    !         *
      *--------!--------!------------------------------------!---------*
      *        !        !                                    !         *
      *        !        !                                    !         *
      ******************************************************************
       01  DRKI3065.
      *
      *    Aktualisierungsskennzeichen  I = Insert U = Update
           05 'XX'-AKTUAL-KZ                    PIC X(001).
      *
      *    Die Gesellschaftsnummer
           05 'XX'-GES                          PIC 9(002).
      *
      *    Die Vertragsnummer mit fuehrenden Nullen
           05 'XX'-VNR                          PIC 9(015).
      *
      *    Verbrauchter Freistellungsbetrag
           05 'XX'-FREI-VERBRAUCHT              PIC 9(05)V99.
           05 'XX'-AEND-FREI-VERBRAUCHT-KZ      PIC X(001).
      *
      *    Nichtveranlagungsbescheinigung-Bis: Datum, bis zu dem die
      *    Zinserträge des Kunde von der Abgeltungssteuer befreit sind.
      *    Format JJJJMMTT
           05 'XX'-DATNVBESCHEIN                PIC X(010).
           05 'XX'-AEND-DATNVBESCHEIN-KZ        PIC X(010).
      *
           05 FILLER                            PIC X(123).

