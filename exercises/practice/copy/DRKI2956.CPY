      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.01.2015
      *            Kategorie  :  Copystrecke
 
      *---------------------------------------------------------------*
      *
 
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
 
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2956.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2956.CPv  $
      *
      *   Rev 1.1   Mar 19 2015 13:10:54   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.0   Jan 14 2015 13:46:58   t13ae31
      *Archiverstellung
      *
 
      *---------------------------------------------------------------**
      *     COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                      *
      *     - AUSGABEDATEI REOB fuer die Tabelle PHOTOVOLTAIK          *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *31.01.15! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *                                                                *
310115******************************************************************
      *                                                                *
      *                522 - P K - P H O T O V O L T A I K             *
      *                                                                *
      *     LAENGE: 400                         STAND: 31.01.2015      *
      *                                                                *
      ******************************************************************
       01 DRKI2956.
                   05  REOB-AUS-522-VERARB-KNZ      PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete
 
                    05 REOB-AUS-522-VERTRAGS-ID        PIC X(13).
 
      ***  Schlüssel: Vertragsnummer FeldNr. 8001
                   05  REOB-AUS-522-CVNR            PIC 9(15).
 
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 8003
                   05  REOB-AUS-522-CVEZHOSTNR      PIC X(15).
 
      ***  Datenfeld: Bedingungen FeldNr. 8011
                   05  REOB-AUS-522-CPVBEDG      PIC X(30).
                   05  REOB-AUS-522-CPVBEDG-UPD  PIC X.
 
      ***  Datenfeld: Mailingaktion FeldNr. 8012
                    05 REOB-AUS-522-CMAILING PIC X(70).
                    05 REOB-AUS-522-CMAILING-UPD  PIC X.
 
      ***  Datenfeld: Zusatzinfo FeldNr. 8013
                    05 REOB-AUS-522-CPVZUSINFO PIC X(100).
                    05 REOB-AUS-522-CPVZUSINFO-UPD  PIC X.
 
      ***  Datenfeld: Informationsquelle FeldNr. 8014
                    05 REOB-AUS-522-LVTQUELLE PIC X(9).
                    05 REOB-AUS-522-LVTQUELLE-UPD  PIC X.
 
      ***  Datenfeld: Baujahr FeldNr. 8015 Form JJJJ
                    05 REOB-AUS-522-CPVBAUJ PIC 9(4).
                    05 REOB-AUS-522-CPVBAUJ-UPD  PIC X.
 
      ***  Datenfeld: Anlagenleistung FeldNr. 8016
                    05 REOB-AUS-522-CPVLEIST PIC 9(4)V99.
                    05 REOB-AUS-522-CPVLEIST-UPD  PIC X.
 
      ***  Datenfeld: Abweichender Anlagenbetreiber FeldNr. 8017
                    05 REOB-AUS-522-CABWANLBTR PIC X(35).
                    05 REOB-AUS-522-CABWANLBTR-UPD  PIC X.
 
      ***  Datenfeld: Mitversicherung von Batteriesystemen
      ***         JA/NEIN    FeldNr. 8018
                    05 REOB-AUS-522-MVBATTSYS PIC XX.
                    05 REOB-AUS-522-MVBATTSYS-UPD  PIC X.
 
      ***  Datenfeld: Gebaeudeart FeldNr. 8019
                    05 REOB-AUS-522-CPVGEBART PIC X(50).
                    05 REOB-AUS-522-CPVGEBART-UPD PIC X.
 
                    05 REOB-AUS-522-INTLFDNR PIC 9(4).
 
                    05 FILLER  PIC X(37).
 

