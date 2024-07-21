      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2077.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2077.CPv  $
      *
      *   Rev 2.7   Jun 17 2020 10:01:46   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.6   Jun 23 2016 10:32:36   t13ae31
      *AKI01438: G10 SUH Angleichung DTA Pgme
      *
      *   Rev 2.5   Mar 07 2011 13:28:40   t13ae31
      *AKI01012: VSP Vorteilsargumentation
      *
      *   Rev 2.4   Nov 11 2010 11:54:50   t13ex14
      *AKI00966: DVAG Österreich: Sach-DTA-Aktualisierung
      *
      *   Rev 2.3   Jun 15 2010 09:50:18   t13ae31
      *AKI00974: VSP 2010
      *
      *   Rev 2.2   13 Nov 2007 11:15:24   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.1   30 Oct 2007 12:18:28   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.0   Sep 17 2007 10:29:04   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle  EINZELVERTRAG        *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
100610*10.06.10! VSP2010 eingebaut                    ! HHU     !      *
      *        !                                      !         !      *
220910*22.09.10! SWI-GESELLSCHAFT                     ! WMI     !      *
220910*        ! f. DVAG Wien eingebaut               !         !      *
      *        !                                      !         !      *
220211*AKI01012|  Vorteilsargumentation eingefügt     |P. Hübner       *
      *        !                                      !         !      *
120516*AKI01438|  neues Feld Produktbez               |P. Hübner!      *
      *
      *AKI01785! neue Felder FLEXN SA401              !P. Hübner!      *
      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 370 BYTES                 STAND: 12.05.2016        *
      **************************************************************** *
 
       01  DRKI2077.
 
                    05  REOB-AUS-401-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete
 
 
                    05 REOB-AUS-401-VERTRAGS-ID        PIC X(13).
 
      ***            VERTRAGSNUMMER
                    05 REOB-AUS-401-CVNR             PIC 9(15).
 
     ***  Schlüssel: VEZ-Host-ID FeldNr. 4064
                  05  REOB-AUS-401-CVEZHOSTNR      PIC X(15).
 
     ***  Datenfeld: VEZ-Sparte FeldNr. 4057
                  05  REOB-AUS-401-NVEZSPARTE      PIC 9(9).
                   05 REOB-AUS-401-NVEZSPARTE-UPD  PIC X.
 
      *            J = Aenderung VEZ-Sparte
     ***  Datenfeld: VEZ-Tarif/Produkt FeldNr. 4058
                  05  REOB-AUS-401-NVEZPBRDNR      PIC 9(9).
                   05 REOB-AUS-401-NVEZPBRDNR-UPD  PIC X.
      *            J = Aenderung VEZ-Tarif/Produkt
 
     ***  Datenfeld: VEZ-Beginn  FeldNr. 4059
                  05  REOB-AUS-401-CVEZBEGINN      PIC X(10).
                  05  REOB-AUS-401-CVEZBEGINN-UPD  PIC X.
     ***           J = Aenderung VEZ-Beginn
 
     ***  Datenfeld: VEZ-Ablauf  FeldNr. 4060
                  05  REOB-AUS-401-CVEZABLAUF      PIC X(10).
                  05  REOB-AUS-401-CVEZABLAUF-UPD  PIC X.
     ***           J = Aenderung VEZ-Ablauf
 
     ***  Datenfeld: VEZ-Status FeldNr. 4061
                   05  REOB-AUS-401-NVEZSTATUS      PIC 9(9).
                   05  REOB-AUS-401-NVEZSTATUS-UPD  PIC X.
      **            J = Aenderung VEZ-STATUS
 
      ***  Datenfeld: VEZ-Bruttobeitrag gem. ZW FeldNr. 4062
                   05  REOB-AUS-401-NVEZBTRG        PIC 9(8)V99.
                   05  REOB-AUS-401-NVEZBTRG-UPD    PIC X.
      **            J = Aenderung VEZ-Bruttobeitrag
 
      ***  Datenfeld: VEZ-Summe FeldNr. 4063
                   05  REOB-AUS-401-NVEZSUMME       PIC 9(10).
                   05  REOB-AUS-401-NVEZSUMME-UPD   PIC X.
      **            J = Aenderung VEZ-SUMME
 
      ***  Datenfeld: Vertragsart FeldNr. 4065
                   05  REOB-AUS-401-NVTART          PIC 9(9).
                   05  REOB-AUS-401-NVTART-UPD      PIC X.
      **            J = Aenderung Vertragsart
 
      ***  Datenfeld: VEZ-Aenderungsgrund FeldNr. 4095
                   05  REOB-AUS-401-CVEZAENDG       PIC X(50).
                   05  REOB-AUS-401-CVEZAENDG-UPD   PIC X.
      **            J = Aenderung VEZ-Aenderungsgrund
 
      ***  Datenfeld: VEZ-Aenderungsdatum  FeldNr. 4096
                   05  REOB-AUS-401-CVEZAENDDT      PIC X(10).
                   05  REOB-AUS-401-CVEZAENDDT-UPD  PIC X.
      *            J = Aenderung VEZ-Aenderungsdatum
 
      ***  Datenfeld: VEZ-Nettojahresbeitrag FeldNr. 4106
                   05  REOB-AUS-401-NVEZNJB         PIC 9(8)V99.
                   05  REOB-AUS-401-NVEZNJB-UPD     PIC X.
      **            J = Aenderung VEZ-Nettojahresbeitrag
 
      ***  Datenfeld: VEZ-Bruttojahresbeitrag FeldNr. 4107
                   05  REOB-AUS-401-NVEZBJB         PIC 9(8)V99.
                   05  REOB-AUS-401-NVEZBJB-UPD     PIC X.
      **            J = Aenderung VEZ-Bruttojahresbeitrag
 
      ***  Datenfeld: VEZ-Nettobeitrag gem. ZW FeldNr. 4108
                   05  REOB-AUS-401-NVEZFBNZW       PIC 9(8)V99.
                   05  REOB-AUS-401-NVEZFBNZW-UPD   PIC X.
      **            J = Aenderung VEZ-Nettobeitrag gem. ZW
 
      ***  Datenfeld: VEZ-Datum der letzten Bearbeitung
      ***             FeldNr. 4109
                   05  REOB-AUS-401-CVEZLBEARB      PIC X(10).
                   05  REOB-AUS-401-CVEZLBEARB-UPD  PIC X.
      **            J = Aenderung VEZ-Datum der letzten Bearbeitung
 
      ***  Datenfeld: Geschäftszweig FeldNr. 4115 ( FK ,  PK )
                   05  REOB-AUS-401-CGZWEIG         PIC XX.
 
100610*** Datenfeld: Rabattierter Jahresnettobeitr. Feldnr. 9301
      ***    inkl. TZ-Zuschlag exkl. Steuer
                   05  REOB-AUS-401-RABNETTO      PIC 9(8)V99.
                   05  REOB-AUS-401-RABNETTO-UPD  PIC X.
      ***  Datenfeld: Summe aller Komponentenbeiträge Feldnr. 9302
                   05  REOB-AUS-401-STAKTBTR      PIC 9(8)V99.
100610             05  REOB-AUS-401-STAKTBTR-UPD  PIC X.
 
220211***  Datenfeld: REOBPRODUKTNUMMER  Feldnr. 4164
220211             05  REOB-AUS-401-NREOBPRDNR    PIC 9(5).
220211             05  REOB-AUS-401-NREOBPRDNR-UPD  PIC X.
 
      *** interne laufende Nummer
                   05  REOB-AUS-401-INTLFDNR       PIC 9(4).
 
      *** ermittelte Produkt-ID
                   05  REOB-AUS-401-PID            PIC 9(4).
                   05  REOB-AUS-401-PID-UPD        PIC X.
      ***            J = Aenderung Produkt-ID
 
      *** entsprechender SUBTYPE
                   05 REOB-AUS-401-SUBTYPE         PIC X.
 
120516             05 REOB-AUS-401-PRODUKTBEZ       PIC X(50).
120516             05 REOB-AUS-401-PRODUKTBEZ-UPD   PIC X(01).
 
                   05 REOB-AUS-401-FLEXNSP          PIC 9(9)V99.
                   05 REOB-AUS-401-FLEXNSP-UPD      PIC X.
                   05 REOB-AUS-401-FLEXNSPGW        PIC X.
                   05 REOB-AUS-401-FLEXNSPGW-UPD    PIC X.
 
120516             05 FILLER                        PIC X(29).
220910*            nur für DVAG-Wien:
220910             05 SWI-GESELLSCHAFT              PIC 99.


