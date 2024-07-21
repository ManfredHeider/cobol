      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2957.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2957.CPv  $
      *
      *   Rev 1.1   Mar 19 2015 13:15:52   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 1.0   Mar 19 2015 09:00:46   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.0   Mar 17 2015 11:12:28   t13ae31
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *     COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                      *
      *     - AUSGABEDATEI REOB fuer die Tabelle FREMDVERSICHERER      *
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
      *             705 - F R V - F R E M D V E R S I C H E R E R      *
      *                                                                *
      *     LAENGE: 300                         STAND: 31.01.2015      *
      *                                                                *
      ******************************************************************
       01 DRKI2957.
                   05  REOB-AUS-705-VERARB-KNZ      PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete
 
                    05 REOB-AUS-705-VERTRAGS-ID        PIC X(13).
 
      ***  Schlüssel: Vertragsnummer FeldNr. 7501
                   05  REOB-AUS-705-CVNR            PIC 9(15).
 
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7502
                   05  REOB-AUS-705-CVEZHOSTNR      PIC X(15).
 
      ***  Schlüssel: Risikogruppe FeldNr. 7504
                   05 REOB-AUS-705-PVRIGRP            PIC 9(4).
 
      ***  Schlüssel: Themengebietsschluessel FeldNr. 7505
                   05 REOB-AUS-705-PTGSCHL           PIC 9(15).
 
      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7506
                   05 REOB-AUS-705-PVRILFDNR         PIC 999.
 
      ***  Datenfeld: Differenzdeckungsgefahr FeldNr. 7511
                    05 REOB-AUS-705-NDIFFGEF PIC X(45).
                    05 REOB-AUS-705-NDIFFGEF-UPD PIC X.
 
      ***  Datenfeld: Differenzdeckungsgesellschaft FeldNr. 7512
                    05 REOB-AUS-705-CGESELL PIC X(50).
                    05 REOB-AUS-705-CGESELL-UPD PIC X.
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 7513
                    05 REOB-AUS-705-CDIFFVSNR PIC X(40).
                    05 REOB-AUS-705-CDIFFVSNR-UPD PIC X.
 
      ***  Datenfeld: Umstellungstermin  FeldNr. 7514
                    05 REOB-AUS-705-CUMSTELLTERM PIC X(10).
                    05 REOB-AUS-705-CUMSTELL-UPD  PIC X.
 
      ***  Datenfeld: Kuendigsunsbestaetigung JA/Nein FeldNr. 7515
                    05 REOB-AUS-705-NKUENDBEST PIC XX.
                    05 REOB-AUS-705-NKUENDB-UPD      PIC X.
 
      ***  Datenfeld: Risikomerkmal FeldNr. 7529
                    05 REOB-AUS-705-RISMNR PIC 9(9).
                    05 REOB-AUS-705-RISMNR-UPD PIC X.
 
                    05 REOB-AUS-705-INTLFDNR PIC 9(4).
 
                    05 FILLER  PIC X(68).
 

