      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2001                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 2001 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRPA1485.CPY  $                                         

      *    $Revision:   2.6  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRPA1485.CPv  $                            
      *
      *   Rev 2.6   Nov 05 2014 14:30:52   t13ex44
      *APA01923: Datenlieferung für DWS Österreich
      *                                                                 
      *   Rev 2.5   Jun 27 2005 10:35:12   t13ex44                      
      *APA00311: DWS - Änderung der Schnittstelle KS025                 
      *                                                                 
      *   Rev 2.4   Jul 21 2004 09:36:02   t13ex44                      
      *APA00311: DWS - Änderung der Schnittstelle KS025                 
      *                                                                 
      *   Rev 2.3   Oct 07 2003 10:41:02   t13ex21                      
      *FPA00634 - Korrektur                                             
      *                                                                 
      *   Rev 2.2   Aug 21 2002 08:58:06   t13ex21                      
      *APA00206 - Erweiterung um Tabellensatz                           
      *                                                                 
      *   Rev 2.1   02 Apr 2001 09:07:24   T13EX21                      
      *APA00131 - Neuerstellung                                         
      *                                                                 
      *   Rev 2.0   09 Jan 2001 10:22:28   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRPA1485  ORIGINAL-PEG-DATENSATZ VON DGI (49) * 
      *    ---------------------------------------------------------  * 
      *    ERSTELLT: STORCH                                           * 
      *    DATUM:    26.03.01                                         * 
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * APA00131!26.03.01!2.1 ! NEUERSTELLUNG             ! STORCH   *  
      * APA00206!03.07.02!2.2 ! ERWEITERUNG UM TABELLENSATZ STORCH   *  
      * FPA00634!06.10.03!2.3 ! KORREKTUR TABELLENSATZ    ! STORCH   *  
      * APA00311!21.06.04!2.4 ! UMSTRUKTURIERUNG GEMAESS  ! SCHEIDEL *  
      *         !        !    ! SST KS025/V3.0            !          *  
      * APA01923!27.10.14!2.5 ! KS025 V7                  ! SCHEIDEL *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                              *  
      *    DIESES COPY ENTHÄLT DEN ORIGINAL-AUFBAU DES PEG-          *  
      *    DATENSATZES DES DGI-DTA'S.                                *  
      ****************************************************************  
      *                                                                 
           30 KS025-DATENSATZ.                                          
              35 KS025-ALLGEMEINER-TEIL.                                
                 40 KS025-SATZART                     PIC  9(003).      
                 40 KS025-KONS-NR                     PIC  9(007).      
                 40 KS025-DEPOT-NR.                                     
                    45 KS025-FONDS-NR                 PIC  9(004).      
                    45 KS025-KUNDEN-NR                PIC  X(008).      
                    45 KS025-DEPOTUNTER-NR            PIC  9(002).      
                 40 KS025-DEPOT-INFO-KZ               PIC  X(001).      
                                                                        
              35 KS025-SPEZIELLER-TEIL                PIC  X(525).      
                                                                        
              35 SA001-VORSATZ REDEFINES KS025-SPEZIELLER-TEIL.         
                 40 SA001-PROGRAMM                    PIC  X(008).      
                 40 SA001-ERST-DATUM                  PIC  9(008).      
                 40 SA001-SELEKT-DATUM-VON            PIC  9(008).      
                 40 SA001-SELEKT-UHRZEIT-VON          PIC  9(007).      
                 40 SA001-SELEKT-DATUM-BIS            PIC  9(008).      
                 40 SA001-SELEKT-UHRZEIT-BIS          PIC  9(007).      
                 40 SA001-ABSENDER-NR                 PIC  9(002).      
                 40 SA001-ABSENDER                    PIC  X(030).      
                 40 SA001-SCHNITTSTELLENVERSION       PIC  9(003)V9(02).
                                                                        
              35 SA020-KUNDENINFORMATION                                
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA020-INHABER-NR                  PIC  9(002).      
                 40 SA020-INHABER-ADRESS-NR           PIC  9(007).      
                 40 SA020-ANREDE                      PIC  9(002).      
                 40 SA020-ADELSTITEL                  PIC  9(002).      
                 40 SA020-AKADEMISCHER-TITEL          PIC  9(002).      
                 40 SA020-VORNAME                     PIC  X(030).      
                 40 SA020-NACHNAME                    PIC  X(030).      
                 40 SA020-STRASSE                     PIC  X(030).      
                 40 SA020-PLZ                         PIC  X(007).      
                 40 SA020-ORT                         PIC  X(030).      
                 40 SA020-ZUSATZTEXT                  PIC  X(030).      
                 40 SA020-INH-DER-KUNDENANSCHRIFT     PIC  9(002).      
                 40 SA020-VERSAND-KZ                  PIC  9(002).      
                 40 SA020-NACHLASS-KZ                 PIC  9(002).      
                 40 SA020-LAND                        PIC  9(003).      
                 40 SA020-GEBURTSDATUM                PIC  9(008).      
                 40 SA020-STERBEDATUM                 PIC  9(008).      
                 40 SA020-BERUF                       PIC  9(003).      
                 40 SA020-KUNDEN-WAEHRUNG             PIC  X(003).      
                 40 SA020-ONLINE-STATUS               PIC  9(001).      
                 40 SA020-RISIKOKLASSE                PIC  9(001).      
                 40 SA020-SPRACHE                     PIC  9(002).      
                 40 SA020-DEPONENT                    PIC  9(003).      
                 40 SA020-STAATSANGEHOERIGKEIT        PIC  9(003).      
                 40 SA020-VERTRAGSSTATUS              PIC  9(002).      
                 40 SA020-GEBURTSNAME                 PIC  X(030).      
                 40 SA020-GEBURTSORT                  PIC  X(030).      
                 40 SA020-GEBURTSLAND                 PIC  9(003).      
                                                                        
              35 SA021-VERSANDADRESSEN                                  
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA021-VERSAND-ZEILE1              PIC  X(030).      
                 40 SA021-VERSAND-ZEILE2              PIC  X(030).      
                 40 SA021-VERSAND-STRASSE             PIC  X(030).      
                 40 SA021-VERSAND-PLZ                 PIC  X(007).      
                 40 SA021-VERSAND-ORT                 PIC  X(030).      
                 40 SA021-VERSAND-KZ                  PIC  9(002).      
                 40 SA021-VERSAND-LAND                PIC  9(003).      
                 40 SA021-VERSAND-ADRESS-NR           PIC  9(007).      
                                                                        
              35 SA022-KUNDEN-ERREICHBARKEIT                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA022-INHABER-NR                  PIC  9(002).      
                 40 SA022-INHABER-ADRESS-NR           PIC  9(007).      
                 40 SA022-GERAET1                     PIC  9(002).      
                 40 SA022-NUMMER1                     PIC  X(050).      
                 40 SA022-GERAET2                     PIC  9(002).      
                 40 SA022-NUMMER2                     PIC  X(050).      
                 40 SA022-GERAET3                     PIC  9(002).      
                 40 SA022-NUMMER3                     PIC  X(050).      
                 40 SA022-GERAET4                     PIC  9(002).      
                 40 SA022-NUMMER4                     PIC  X(050).      
                 40 SA022-GERAET5                     PIC  9(002).      
                 40 SA022-NUMMER5                     PIC  X(050).      
                 40 SA022-GERAET6                     PIC  9(002).      
                 40 SA022-NUMMER6                     PIC  X(050).      
                 40 SA022-GERAET7                     PIC  9(002).      
                 40 SA022-NUMMER7                     PIC  X(050).      
                                                                        
              35 SA023-KUNDEN-ERREICHBARKEIT-2                          
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA023-INHABER-NR                  PIC  9(002).      
                 40 SA023-GERAET8                     PIC  9(002).      
                 40 SA023-NUMMER8                     PIC  X(050).      
                 40 SA023-GERAET9                     PIC  9(002).      
                 40 SA023-NUMMER9                     PIC  X(050).      
                 40 SA023-GERAET10                    PIC  9(002).      
                 40 SA023-NUMMER10                    PIC  X(050).      
                                                                        
              35 SA024-DB-VITA-KUNDENZUSATZ                             
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA024-VERTRAGSBEGINN              PIC  9(008).      
                 40 SA024-VERTRAGSENDE                PIC  9(008).      
                 40 SA024-BEITRAGSSUMME               PIC  9(015)V9(02).
                                                                        
              35 SA025-VERTRAGSINFORMATION                              
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA025-INHABER-NR                  PIC  9(002).      
                 40 SA025-SOZIALVERS-NR               PIC  X(012).      
                 40 SA025-EUSD-OPTION                 PIC  X(001).      
                 40 SA025-TIN                         PIC  X(030).      
                 40 SA025-RECHTSSSUBJEKT              PIC  9(002).      
                 40 SA025-AUSWEISART                  PIC  9(002).      
                 40 SA025-AUSWEIS-NR                  PIC  X(020).      
                 40 SA025-GUELTIG-BIS                 PIC  9(008).      
                 40 SA025-US-TIN                      PIC  X(020).      
                 40 SA025-FACTA-STATUS                PIC  9(002).      
                 40 SA025-NACHWEIS                    PIC  9(008).      
                 40 SA025-STEUERLAND                  PIC  9(003).      
                 40 SA025-FREMDBESTAND                PIC  X(001).      
                                                                        
              35 SA026-WIRTSCH-BERECHTIGTE                              
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA026-VOLLMACHTSART               PIC  9(002).      
                 40 SA026-VOLLMACHTSNR                PIC  9(002).      
                 40 SA026-ANREDE                      PIC  9(002).      
                 40 SA026-ADELSTITEL                  PIC  9(002).      
                 40 SA026-AKADEMISCHER-TITEL          PIC  9(002).      
                 40 SA026-VORNAME                     PIC  X(030).      
                 40 SA026-NACHNAME                    PIC  X(030).      
                 40 SA026-ZUSATZTEXT                  PIC  X(030).      
                                                                        
              35 SA027-REFERENZBANKVERBINDUNG                           
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA027-IBAN                        PIC  X(032).      
                 40 SA027-BIC                         PIC  X(011).      
                 40 SA027-BANKVERBINDUNG              PIC  X(040).      
                                                                        
              35 SA028-MANDAT                                           
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA028-MANDATSREFERENZ             PIC  X(010).      
                 40 SA028-MAN-DEP-IDENTISCH           PIC  X(001).      
                 40 SA028-KONTOINHABER                PIC  X(070).      
                 40 SA028-IBAN                        PIC  X(032).      
                 40 SA028-BIC                         PIC  X(011).      
                 40 SA028-BANKVERBINDUNG              PIC  X(040).      
                 40 SA028-LASTSCHRIFTTYP              PIC  9(002).      
                 40 SA028-STATUS                      PIC  9(002).      
                 40 SA028-GUELTIB-AB                  PIC  9(008).      
                 40 SA028-ERSTE-NUTZUNG               PIC  9(008).      
                 40 SA028-LETZTE-NUTZUNG              PIC  9(008).      
                 40 SA028-EINMALIGES-MANDAT           PIC  X(001).      
                                                                        
              35 SA030-VERTRAGSINFORMATION                              
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA030-VINFO.                                        
                    45 FILLER                         PIC  X(009).      
                    45 SA030-VINFO-INTVSNR            PIC  9(007).      
                    45 SA030-VINFO-I-PRZ              PIC  9(002).      
                    45 SA030-VINFO-VBNR               PIC  9(007).      
                    45 SA030-VINFO-VB-PRZ             PIC  9(001).      
                    45 FILLER                         PIC  X(074).      
                 40 SA030-VERMITTLER-NR               PIC  X(020).      
                 40 SA030-PORTFOLIO-NR                PIC  9(002).      
                 40 SA030-TARIF                       PIC  9(002).      
                 40 SA030-VERTRAGSART                 PIC  9(002).      
                 40 SA030-DYNAMISCHE-PROZ             PIC  9(003)V9(02).
                 40 SA030-SPERR-KZ                    PIC  9(002).      
                 40 SA030-BESTAND                     PIC  9(013)V9(04).
                 40 SA030-GEGENWERT                   PIC  9(015)V9(02).
                 40 SA030-BEGINN                      PIC  9(008).      
                 40 SA030-SPARZIEL                    PIC  9(015)V9(02).
                 40 SA030-RATE                        PIC  9(015)V9(02).
                 40 SA030-ERSTE-ZAHLUNG               PIC  9(008).      
                 40 SA030-LAUFZEIT                    PIC  9(002).      
                 40 SA030-UMSCHICHTUNG                PIC  X(001).      
                 40 SA030-KUM-EINZAHLUNG-NEG          PIC  X(001).      
                 40 SA030-KUM-EINZAHLUNG              PIC  9(015)V9(02).
                 40 SA030-KUM-AUSZAHLUNG-NEG          PIC  X(001).      
                 40 SA030-KUM-AUSZAHLUNG              PIC  9(015)V9(02).
                 40 SA030-KUM-AUSSCHUETTUNG-NEG       PIC  X(001).      
                 40 SA030-KUM-AUSSCHUETTUNG           PIC  9(015)V9(02).
                 40 SA030-ERSTE-ZAHLUNG-VL1           PIC  9(008).      
                 40 SA030-ERSTE-ZAHLUNG-VL2           PIC  9(008).      
                 40 SA030-NACHKOMMASTELLEN            PIC  9(001).      
                 40 SA030-DWS-DEPOT-PLUS              PIC  X(001).      
                                                                        
              35 SA031-ERG-INFOS-VLVERTRAG                              
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA031-ERSTE-ZAHLUNG-VL1           PIC  9(008).      
                 40 SA031-ERSTE-ZAHLUNG-VL2           PIC  9(008).      
                 40 SA031-GESP-ANTEILE-VL1-NEG        PIC  X(001).      
                 40 SA031-GESP-ANTEILE-VL1            PIC  9(013)V9(04).
                 40 SA031-GESP-ANTEILE-VL2-NEG        PIC  X(001).      
                 40 SA031-GESP-ANTEILE-VL2            PIC  9(013)V9(04).
                 40 SA031-FREIE-ANTEILE-VL1-NEG       PIC  X(001).      
                 40 SA031-FREIE-ANTEILE-VL1           PIC  9(013)V9(04).
                 40 SA031-FREIE-ANTEILE-VL2-NEG       PIC  X(001).      
                 40 SA031-FREIE-ANTEILE-VL2           PIC  9(013)V9(04).
                 40 SA031-ENDE-EINZFRIST-VL1          PIC  9(008).      
                 40 SA031-ENDE-EINZFRIST-VL2          PIC  9(008).      
                 40 SA031-ENDE-FESTLFRIST-VL1         PIC  9(008).      
                 40 SA031-ENDE-FESTLFRIST-VL2         PIC  9(008).      
                 40 SA031-FREIE-ANTEILE-GES-NEG       PIC  X(001).      
                 40 SA031-FREIE-ANTEILE-GES           PIC  9(013)V9(04).
                                                                        
                                                                        
              35 SA040-POSITIONSINFORMATIONEN                           
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA040-BELEG-NR                    PIC  9(009).      
                 40 SA040-BELEGUNTER-NR               PIC  9(002).      
                 40 SA040-BELEG-POS                   PIC  9(005).      
                 40 SA040-UMSATZ-SCHL                 PIC  9(003).      
                 40 SA040-SOLL-HABEN-KZ               PIC  X(001).      
                 40 SA040-PREISTAG                    PIC  9(008).      
                 40 SA040-PREIS                       PIC  9(007)V9(06).
                 40 SA040-STORNO-KZ                   PIC  X(001).      
                 40 SA040-NETTO-BETRAG-NEG            PIC  X(001).      
                 40 SA040-NETTO-BETRAG                PIC  9(015)V9(02).
                 40 SA040-BRUTTO-BETRAG-NEG           PIC  X(001).      
                 40 SA040-BRUTTO-BETRAG               PIC  9(015)V9(02).
                 40 SA040-ANTEILE                     PIC  9(013)V9(04).
                 40 SA040-TARIF                       PIC  9(002).      
                 40 SA040-BONI-AN-KONSORTE-NEG        PIC  X(001).      
                 40 SA040-BONI-AN-KONSORTE            PIC  9(015)V9(02).
                 40 SA040-BONI-AN-KONSINKWAEH-NEG     PIC  X(001).      
                 40 SA040-BONI-AN-KONSINKWAEH         PIC  9(015)V9(02).
                 40 SA040-RABATT                      PIC  9(003)V9(02).
                 40 SA040-VERB-DATUM                  PIC  9(008).      
                 40 SA040-VERB-UHRZEIT                PIC  9(007).      
                 40 SA040-GEGENREFERENZ               PIC  9(016).      
                 40 SA040-VERBR-FREIST-AKT-NEG        PIC  X(001).      
                 40 SA040-VERBR-FREIST-AKT            PIC  9(015)V9(02).
                 40 SA040-BESTAND-AKTUELL             PIC  9(013)V9(04).
                 40 SA040-TEXTSCHLUESSEL              PIC  9(005).      
                 40 SA040-KUM-EINZAHLUNG-AKT-NEG      PIC  X(001).      
                 40 SA040-KUM-EINZAHLUNG-AKT          PIC  9(015)V9(02).
                 40 SA040-KUM-AUSZAHLUNG-AKT-NEG      PIC  X(001).      
                 40 SA040-KUM-AUSZAHLUNG-AKT          PIC  9(015)V9(02).
                 40 SA040-KUM-AUSSCHUETT-AKT-NEG      PIC  X(001).      
                 40 SA040-KUM-AUSSCHUETT-AKT          PIC  9(015)V9(02).
                 40 SA040-UMRECHNUNGSKURS             PIC  9(007)V9(06).
                 40 SA040-UMRECHNUNGSKURSBASIS        PIC  9(007)V9(06).
                 40 SA040-URSPRUNGS-WAEHRUNG          PIC  X(003).      
                 40 SA040-ZIEL-WAEHRUNG               PIC  X(003).      
                 40 SA040-EXT-NR                      PIC  9(009).      
                 40 SA040-ZWIST-AKTUELL-NEG           PIC  X(001).      
                 40 SA040-ZWIST-AKTUELL               PIC  9(015)V9(02).
                 40 SA040-ANT-TILGBETRAG-NEG          PIC  X(001).      
                 40 SA040-ANT-TILGBETRAG              PIC  9(015)V9(02).
                 40 SA040-BEITRAGSJAHR                PIC  9(004).      
                 40 SA040-RETOUREGRUND                PIC  9(001).      
                 40 SA040-REF-BELEG-UMTAUSCH          PIC  9(016).      
                 40 SA040-GARANTIE-VERAEND-NEG        PIC  X(001).      
                 40 SA040-GARANTIE-VERAEND            PIC  9(015)V9(02).
                                                                        
              35 SA041-MIFID-INFO                                       
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA041-BELEG-NR                    PIC  9(009).      
                 40 SA041-BELEG-UNTERNR               PIC  9(002).      
                 40 SA041-BELEG-POS                   PIC  9(005).      
                 40 SA041-MIFID-TYP-1                 PIC  9(002).      
                 40 SA041-INFO-TEXT-1                 PIC  X(030).      
                 40 SA041-INFO-BETRAG-1               PIC  9(015)V9(02).
                 40 SA041-INFO-PREIS-1                PIC  9(007)V9(06).
                 40 SA041-MIFID-TYP-2                 PIC  9(002).      
                 40 SA041-INFO-TEXT-2                 PIC  X(030).      
                 40 SA041-INFO-BETRAG-2               PIC  9(015)V9(02).
                 40 SA041-INFO-PREIS-2                PIC  9(007)V9(06).
                 40 SA041-MIFID-TYP-3                 PIC  9(002).      
                 40 SA041-INFO-TEXT-3                 PIC  X(030).      
                 40 SA041-INFO-BETRAG-3               PIC  9(015)V9(02).
                 40 SA041-INFO-PREIS-3                PIC  9(007)V9(06).
                 40 SA041-MIFID-TYP-4                 PIC  9(002).      
                 40 SA041-INFO-TEXT-4                 PIC  X(030).      
                 40 SA041-INFO-BETRAG-4               PIC  9(015)V9(02).
                 40 SA041-INFO-PREIS-4                PIC  9(007)V9(06).
                 40 SA041-MIFID-TYP-5                 PIC  9(002).      
                 40 SA041-INFO-TEXT-5                 PIC  X(030).      
                 40 SA041-INFO-BETRAG-5               PIC  9(015)V9(02).
                 40 SA041-INFO-PREIS-5                PIC  9(007)V9(06).
                                                                        
              35 SA042-AEND-INFO-BUCHPOS                                
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA042-BELEG-NR                    PIC  9(009).      
                 40 SA042-BELEGUNTER-NR               PIC  9(002).      
                 40 SA042-BELEB-POS                   PIC  9(005).      
                 40 SA042-BEITRAGSJAHR                PIC  9(004).      
                                                                        
              35 SA045-VERTRIEBSPROVISION                               
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA045-VERTRIEBSPROV-DATUM         PIC  9(008).      
                 40 SA045-VERTRIEBSPROV-ANTEILE       PIC  9(013)V9(04).
                 40 SA045-VERTRIEBSPROV-RUECKPR       PIC  9(007)V9(06).
                 40 SA045-VERTRIEBSPROV-PROZENT       PIC  9(003)V9(04).
                 40 SA045-VERTRIEBSPROV-VERWALT       PIC  9(003)V9(04).
                 40 SA045-VERTRIEBSPROV-BASISPKT      PIC  9(003)V9(04).
                 40 SA045-VERTRIEBSPROV-IN-FWAEH      PIC  9(015)V9(02).
                 40 SA045-VERTRIEBSPROV-FONDSPR       PIC  9(008).      
                                                                        
              35 SA048-WAEHRUNGSENDESATZ                                
                     REDEFINES KS025-SPEZIELLER-TEIL.                   
                 40 SA048-SUMME-WAEHRUNG              PIC  X(003).      
                 40 SA048-SUMME-BRUTTO-BETRAG         PIC  9(015)V9(02).
                 40 SA048-SUMME-BONI-AN-KONS          PIC  9(015)V9(02).
                 40 SA048-SUMME-BONI-AN-KONS-KWAE     PIC  9(015)V9(02).
                 40 SA048-SUMME-BETRAG-BEI-BONI       PIC  9(015)V9(02).
                 40 SA048-SUMME-VERTRIEBSPROV         PIC  9(015)V9(02).
                 40 SA048-SUMME-VERTRAGBSPROV-KWAE    PIC  9(015)V9(02).
                                                                        
              35 SA108-FREISTELLUNGSAUFTRAG                             
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA108-FREIST-BETRAG               PIC  9(015)V9(02).
                 40 SA108-FREIST-BETRAG-GUELT-VON     PIC  9(008).      
                 40 SA108-FREIST-BETRAG-GUELT-BIS     PIC  9(008).      
                 40 SA108-VERBR-FREIST-BETRAG-NEG     PIC  X(001).      
                 40 SA108-VERBR-FREIST-BETRAG         PIC  9(015)V9(02).
                 40 SA108-FREIST-BETRAG-GUELT-VORH    PIC  9(008).      
                 40 SA108-ZWIST-AUFGEL-BETRAG-NEG     PIC  X(001).      
                 40 SA108-ZWIST-AUFGEL-BETRAG         PIC  9(015)V9(02).
                                                                        
              35 SA111-PORTFOLIO                                        
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA111-PORTFOLIO-MODELL-NR         PIC  9(007).      
                 40 SA111-PORTFOLIO-MDELL-KONS-NR     PIC  9(007).      
                 40 SA111-PORTFOLIO-MODELL-VERBIND    PIC  9(002).      
                 40 SA111-PORTFOLIO-MODELL-STATUS     PIC  9(002).      
                 40 SA111-PROTFOLIO-TEXT              OCCURS 004        
                                                      PIC  X(030).      
                 40 SA111-VERMITTLER-NR               PIC  X(020).      
                 40 SA111-VERTRAGSART                 PIC  9(002).      
                 40 SA111-GEBUEHR-NR                  PIC  9(002).      
                 40 SA111-TARIF                       PIC  9(002).      
                 40 SA111-ABRECHNUNGS-RHYTHMUS        PIC  9(002).      
                 40 SA111-KAUF-MODUS                  PIC  9(002).      
                 40 SA111-AKTIVIERUNGSDATUM           PIC  9(008).      
                 40 SA111-SPERR-KZ                    PIC  9(002).      
                 40 SA111-FONDS-NR                    OCCURS 015        
                                                      PIC  9(004).      
                 40 SA111-INV-KTO-UNR                 OCCURS 015        
                                                      PIC  9(004).      
                 40 SA111-PROZENTSATZ                 OCCURS 015        
                                                      PIC  9(003)V9(02).
                 40 SA111-VINFO                       PIC  X(100).      
                                                                        
              35 SA112-PORTFOLIO-ERWEITERUNG                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA112-FONDS-NR                    OCCURS 030        
                                                      PIC  9(004).      
                 40 SA112-INV-KTO-UNR                 OCCURS 030        
                                                      PIC  9(004).      
                 40 SA112-PROZENTSATZ                 OCCURS 030        
                                                      PIC  9(003)V9(02).
                                                                        
              35 SA113-PORTFOLIO-ERWEITERUNG                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA113-FONDS-NR                    OCCURS 030        
                                                      PIC  9(004).      
                 40 SA113-INV-KTO-UNR                 OCCURS 030        
                                                      PIC  9(004).      
                 40 SA113-PROZENTSATZ                 OCCURS 030        
                                                      PIC  9(003)V9(02).
                                                                        
              35 SA114-PORTFOLIO-ERWEITERUNG                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA114-FONDS-NR                    OCCURS 025        
                                                      PIC  9(004).      
                 40 SA114-INV-KTO-UNR                 OCCURS 025        
                                                      PIC  9(004).      
                 40 SA114-PROZENTSATZ                 OCCURS 025        
                                                      PIC  9(003)V9(02).
                                                                        
              35 SA120-REGELMAESSIGE-AUSZAHLUNG                         
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA120-KONTOINHABER                PIC  X(070).      
                 40 SA120-IBAN                        PIC  X(032).      
                 40 SA120-BIC                         PIC  X(011).      
                 40 SA120-ENTNAHME-NR                 PIC  9(002).      
                 40 SA120-ENTNAHME-GUELTIG-VON        PIC  9(008).      
                 40 SA120-ENTNAHME-GUELTIG-BIS        PIC  9(008).      
                 40 SA120-ENTNAHME-BETRAG             PIC  9(015)V9(02).
                 40 SA120-ENTNAHME-TERMIN             PIC  9(002).      
                 40 SA120-ENTNAHME-RHYTHMUS           PIC  9(002).      
                 40 SA120-ENTNAHME-WAEHRUNG           PIC  X(003).      
                 40 SA120-ENTNAHME-ANTEILE            PIC  9(013)V9(04).
                                                                        
              35 SA130-REGELMAESSIG-EINZAHLUNG                          
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA130-PORTFOLIO-NR                PIC  9(002).      
                 40 SA130-MANDATSREFERENZ             PIC  X(010).      
                 40 SA130-LASTSCHRIFT-NR              PIC  9(002).      
                 40 SA130-LASTSCHRIFT-GUELT-VON       PIC  9(008).      
                 40 SA130-LASTSCHRIFT-GUELT-BIS       PIC  9(008).      
                 40 SA130-LASTSCHRIFT-BETRAG          PIC  9(015)V9(02).
                 40 SA130-LASTSCHRIFT-TERMIN          PIC  9(002).      
                 40 SA130-LASTSCHRIFT-RHYTHMUS        PIC  9(002).      
                 40 SA130-LASTSCHRIFT-DYN-PROZ        PIC  9(003)V9(02).
                 40 SA130-LASTSCHRIFT-BASISBETR       PIC  9(015)V9(02).
                                                                        
              35 SA194-ZULAGENSTAMM                                     
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA194-BEITRAGSJAHR                PIC  9(004).      
                 40 SA194-BEITRAGSSTATUS              PIC  9(004).      
                 40 SA194-UNMITTELB-BERECHTIGTER      PIC  X(001).      
                 40 SA194-EHEGATTEN-DATEN-VORH        PIC  X(001).      
                 40 SA194-AV-VERAENDERUNG-NEG         PIC  X(001).      
                 40 SA194-AV-VERAENDERUNG             PIC  9(015)V9(02).
                 40 SA194-BEAMTER                     PIC  X(001).      
                 40 SA194-KIND-ZULAGE-BEANTRAGT       PIC  X(001).      
                 40 SA194-KIND-ZULAGE-GEWAEHRT        PIC  9(002).      
                 40 SA194-DAUERZULAGEANTRAG           PIC  X(001).      
                 40 SA194-DAUERZULA-ANTR-DATUM        PIC  9(008).      
                 40 SA194-DATUM-BS-P10-VOLLM          PIC  9(008).      
                 40 SA194-AUSLAND-PFLICHT-VERS        PIC  X(001).      
                 40 SA194-ALTERSKASSE-MITGL-NR        PIC  9(011).      
                 40 SA194-GEZAHLTE-ZULAGEN            PIC  9(015)V9(02).
                 40 SA194-KEIN-ZULAGEANTRAG           PIC  X(001).      
                 40 SA194-AUFTRAGSDATUM               PIC  9(008).      
                                                                        
              35 SA195-ZULAGENSTAMM-KINDER                              
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA195-BEITRAGSJAHR                PIC  9(004).      
                 40 SA195-VORNAME-KIND                PIC  X(030).      
                 40 SA195-NACHNAME-KIND               PIC  X(030).      
                 40 SA195-GEBURTSDATUM-KIND           PIC  9(008).      
                                                                        
              35 SA196-ERMITTLUNGSERGEBNIS                              
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA196-BEITRAGSJAHR                PIC  9(004).      
                 40 SA196-MELDUNG-GRUND               PIC  X(004).      
                 40 SA196-ERSTELL-DATUM               PIC  9(008).      
                 40 SA196-ERSTELL-UHRZEIT             PIC  9(007).      
                 40 SA196-ERSTELL-DATUM-ERST          PIC  9(008).      
                 40 SA196-ERSTELL-UHRZEIT-ERST        PIC  9(007).      
                 40 SA196-ERSTELL-DATUM-V-AEND        PIC  9(008).      
                 40 SA196-ERSTELL-UHRZEIT-V-AEND      PIC  9(007).      
                 40 SA196-ANTRAG-DATUM                PIC  9(008).      
                 40 SA196-KEIN-ZULAGEN-LZ             PIC  9(002).      
                 40 SA196-ZULAGEN-KUERZ-KZ            PIC  9(002).      
                                                                        
              35 SA197-SCHAEDLICHE-VERWENDUNG                           
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA197-BEARB-STATUS                PIC  9(004).      
                 40 SA197-ERSTELL-DATUM               PIC  9(008).      
                 40 SA197-ERSTELL-UHRZEIT             PIC  9(007).      
                 40 SA197-MERKM-SCHAEDL-VERW          PIC  9(002).      
                 40 SA197-DATUM-SCHAEDL-VERW          PIC  9(008).      
                                                                        
              35 SA198-KAPITAL-UEBERTRAGUNG                             
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA198-BEARB-STATUS                PIC  9(004).      
                 40 SA198-ERSTELL-DATUM               PIC  9(008).      
                 40 SA198-ERSTELL-UHRZEIT             PIC  9(007).      
                 40 SA198-UEBERTRAGUNGSWEG            PIC  X(001).      
                 40 SA198-UEBERTRAGUNGSART            PIC  X(001).      
                 40 SA198-PROZ-ANT-KAP-UEBERTR        PIC  9(003)V9(02).
                 40 SA198-DATUM-KAP-UEBERTR           PIC  9(008).      
                 40 SA198-DATUM-GELDBEWEGUNG          PIC  9(008).      
                 40 SA198-ANBIETER-NEU                PIC  X(010).      
                 40 SA198-ZERTIG-ANBIETER-NEU         PIC  X(006).      
                 40 SA198-ANBIETER-ALT                PIC  X(010).      
                 40 SA198-ZERTIF-ANBIETER-ALT         PIC  X(006).      
                 40 SA198-UEBETRAGUNGS-KZ             PIC  9(003).      
                 40 SA198-ANBIETERNAME-NEU            PIC  X(040).      
                 40 SA198-ANBIETERNAME-ALT            PIC  X(040).      
                 40 SA198-VERTR-NR-NEU                PIC  X(020).      
                 40 SA198-VERTR-NR-ALT                PIC  X(020).      
                                                                        
              35 SA199-AUSZAHLUNGSPHASE                                 
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA199-BEARB-STATUS                PIC  9(004).      
                 40 SA199-LEIBR-LEIST-BEGINN          PIC  9(008).      
                 40 SA199-LEIBR-GARANTIEZEIT          PIC  X(001).      
                 40 SA199-LEIBR-GARANTIEDAUER         PIC  9(002).      
                 40 SA199-AUSZAHLUNGSPHASE-BEG        PIC  9(008).      
                 40 SA199-GARANTIERENTE-AKTUELL       PIC  9(015)V9(02).
                 40 SA199-UEBERSCH-ANT-AKTUELL        PIC  9(015)V9(02).
                                                                        
              35 SA200-WOHNRIESTER-ENTNAHME                             
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA200-BESCHEID-ART                PIC  9(002).      
                 40 SA200-BEARB-STATUS-ENTNAHME       PIC  9(004).      
                 40 SA200-ANTRAGS-DATUM               PIC  9(008).      
                 40 SA200-ERSTELL-DATUM-ENTNAHME      PIC  9(008).      
                 40 SA200-ERSTELL-UHRZEIT-ENTNA       PIC  9(007).      
                 40 SA200-BESCHEID-DATUM              PIC  9(008).      
                 40 SA200-AUSZ-BARER-AV-EIG-100       PIC  9(015)V9(02).
                 40 SA200-AUSZ-BARER-AV-EIG-75        PIC  9(015)V9(02).
                 40 SA200-AUSZ-BARER-AV-EIG-BEGR      PIC  9(015)V9(02).
                                                                        
              35 SA201-WOHNRIESTER-AUSZAHLUNG                           
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA201-BEARB-STATUS-AUSZAHLUNG     PIC  9(004).      
                 40 SA201-ERSTELL-DATUM-AUSZAHLUNG    PIC  9(008).      
                 40 SA201-ERSTELL-UHRZEIT-AUSZAHL     PIC  9(007).      
                 40 SA201-ERSTELL-DATUM-ENTNAME       PIC  9(008).      
                 40 SA201-ERSTELL-UHRZEIT-ENTN        PIC  9(007).      
                 40 SA201-AUSZAHLUNGS-DATUM           PIC  9(008).      
                 40 SA201-GES-GEFOERDERTES-KAP        PIC  9(015)V9(02).
                 40 SA201-AUSZUZAHLENDER-BETRAG       PIC  9(015)V9(02).
                 40 SA201-AUSZAHLUNG-PROZENT          PIC  9(003)V9(02).
                 40 SA201-ENTNAHME-BETRAG             PIC  9(015)V9(02).
                 40 SA201-AZ06-ERSTELL-DATUM          PIC  9(008).      
                                                                        
              35 SA202-WOHNFOERDERKONTO                                 
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA202-SUMME-WOHNFOERDERKONTO      PIC  9(015)V9(02).
                                                                        
              35 SA300-ZUSATZINFORMATION                                
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA300-PORTFOLIO-NR                PIC  9(002).      
                 40 SA300-RATE                        PIC  9(015)V9(02).
                 40 SA300-ERSTE-ZAHLUNG               PIC  9(008).      
                 40 SA300-LAUFZEIT                    PIC  9(002).      
                 40 SA300-PORT-AUSGABEAUFSCHLAG       PIC  9(003)V9(02).
                                                                        
              35 SA301-VERTR-INFO-DWS-VORSORGE                          
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA301-VERTRAGSBEGINN              PIC  9(008).      
                 40 SA301-VERTR-JAHRESRATE-1          PIC  9(015)V9(02).
                 40 SA301-VERTR-JAHRESRATE-2          PIC  9(015)V9(02).
                 40 SA301-JAHRESRATE-1                PIC  9(015)V9(02).
                 40 SA301-JAHRESRATE-2                PIC  9(015)V9(02).
                 40 SA301-RIESTERTREPPE               PIC  X(001).      
                 40 SA301-PRODUKTPROVISION            PIC  9(003)V9(02).
                 40 SA301-VERTR-REFINANZ-KST          PIC  9(003)V9(02).
                 40 SA301-GAP-KTO-VORVERPR-ZAHL       PIC  9(015)V9(02).
                 40 SA301-GAP-KTO-UEBERZAHL           PIC  9(015)V9(02).
                 40 SA301-TILGUNGSSATZ                PIC  9(003)V9(04).
                 40 SA301-TILGUNGSBETRAG              PIC  9(015)V9(02).
                 40 SA301-STORNORISIKO                PIC  9(015)V9(02).
                 40 SA301-MAHNSTUFE                   PIC  9(003).      
                 40 SA301-BEITRAGSSUMME               PIC  9(015)V9(02).
                 40 SA301-ENDALTER                    PIC  9(002).      
                 40 SA301-ERW-UEBERW-BETRAG           PIC  9(015)V9(02).
                 40 SA301-VERTRAGSENDE                PIC  9(008).      
                 40 SA301-GARANTIESUMME               PIC  9(015)V9(02).
                 40 SA301-HOECHSTSTAND                PIC  9(015)V9(02).
                 40 SA301-AENDERUNG-HOECHSTSTAND      PIC  9(008).      
                 40 SA301-BEGINN-HOECHSTSTAND         PIC  9(008).      
                 40 SA301-ENDE-HOECHSTSTAND           PIC  9(008).      
                 40 SA301-ABSCHLUSS-KONS-NR           PIC  9(007).      
                 40 SA301-ABSCHLUSS-VINFO             PIC  X(100).      
                 40 SA301-ABLAUFSTABI-GEWAEHLT        PIC  X(001).      
                 40 SA301-BEGINN-ABLAUFSTABI          PIC  9(008).      
                 40 SA301-RECHTSSTAND                 PIC  X(001).      
                 40 SA301-DATUM-SCHAEDL-VERWEND       PIC  9(008).      
                 40 SA301-BEITRAGSDAUER               PIC  9(002).      
                 40 SA301-BEITRAGSENDE                PIC  9(008).      
                                                                        
              35 SA400-KONSORTENINFORMATION                             
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA400-KONSORTENGRUPPE             PIC  9(003).      
                 40 SA400-KONSORTENBEZIRK             PIC  9(003).      
                 40 SA400-EROEFFNUNGSGRUPPE           PIC  9(003).      
                 40 SA400-VERTRIEBS-PROV-GRUPPE       PIC  9(003).      
                 40 SA400-ANREDE                      PIC  9(002).      
                 40 SA400-ADELSTITEL                  PIC  9(002).      
                 40 SA400-AKAD-TITEL                  PIC  9(002).      
                 40 SA400-INSTITUT                    PIC  X(030).      
                 40 SA400-FILIALE                     PIC  X(030).      
                 40 SA400-STRASSE                     PIC  X(030).      
                 40 SA400-PLZ                         PIC  X(007).      
                 40 SA400-ORT                         PIC  X(030).      
                 40 SA400-VERSAND-KZ                  PIC  9(002).      
                 40 SA400-LAND                        PIC  9(003).      
                 40 SA400-ZUSATZTEXT                  PIC  X(030).      
                 40 SA400-WAEHRUNG                    PIC  X(003).      
                 40 SA400-ONLINE-KZ                   PIC  9(002).      
                                                                        
              35 SA401-KONSORTENINFORMATION-ERW                         
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA401-IBAN                        PIC  X(032).      
                 40 SA401-BIC                         PIC  X(011).      
                 40 SA401-BANKVERBINDUNG              PIC  X(040).      
                 40 SA401-TEXT1                       PIC  X(040).      
                 40 SA401-TEXT2                       PIC  X(040).      
                                                                        
              35 SA402-VERMITTLUNGSPROVISION                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA402-TARIF                       PIC  9(002).      
                 40 SA402-PROZENTSATZ-KONSORTE        PIC  9(003)V9(02).
                 40 SA402-PROZENTSATZ-DEPOT           PIC  9(003)V9(02).
                 40 SA402-BERECHNUNGSMETHODE          PIC  9(002).      
                 40 SA402-ZUORDNUNGSTABELLE           PIC  9(003).      
                                                                        
              35 SA404-LFD-VERMITT-PROVISION                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA404-PROZENTSATZ                 PIC  9(003)V9(04).
                 40 SA404-BASISPUNKTE                 PIC  9(003)V9(04).
                                                                        
              35 SA405-PROVISIONSABR-ZEITRAEUME                         
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA405-VERMITT-PROV-VON-DATUM      PIC  9(008).      
                 40 SA405-VERMITT-PROV-VON-UHRZEIT    PIC  9(007).      
                 40 SA405-VERMITT-PROV-BIS-DATUM      PIC  9(008).      
                 40 SA405-VERMITT-PROV-BIS-UHRZEIT    PIC  9(007).      
                 40 SA405-LFD-VERM-PROV-VON-DATUM     PIC  9(008).      
                 40 SA405-LFD-VERM-PROV-VON-UHRZ      PIC  9(007).      
                 40 SA405-LFD-VERM-PROV-BIS-DATUM     PIC  9(008).      
                 40 SA405-LFD-VERM-PROV-BIS-UHRZ      PIC  9(007).      
                                                                        
              35 SA406-PRODUKT-PROV-ZUORDNUNG                           
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA406-TARIF                       PIC  9(002).      
                 40 SA406-PROZENTSATZ-KONSORTE        PIC  9(003)V9(02).
                 40 SA406-PROZENTSATZ-INVEST-KTO      PIC  9(003)V9(02).
                 40 SA406-BERECHNUNGSMETHODE          PIC  9(002).      
                 40 SA406-TARIF-SPERR-KZ              PIC  9(002).      
                                                                        
              35 SA420-PORTFOLIO-MODELL                                 
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA420-NUMMER                      PIC  9(007).      
                 40 SA420-BEZEICHNUNG                 OCCURS 004        
                                                      PIC  X(030).      
                 40 SA420-STATUS                      PIC  9(002).      
                 40 SA420-GLOBAL-STATUS               PIC  9(002).      
                 40 SA420-ANLAGE-HORIZONT             PIC  9(002).      
                 40 SA420-ANLAGE-ERFAHRUNG            PIC  9(002).      
                 40 SA420-RISIKOKLASSE                PIC  9(001).      
                 40 SA420-PRIORITAET                  PIC  9(002).      
                 40 SA420-VERMITTLER-NR               PIC  X(020).      
                 40 SA420-FONDS-NR                    OCCURS 020        
                                                      PIC  9(004).      
                 40 SA420-PROZENTSATZ                 OCCURS 020        
                                                      PIC  9(003)V9(02).
                                                                        
              35 SA421-PORTFOLIO-ERWEITERUNG                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA421-NUMMER                      PIC  9(007).      
                 40 SA421-FONDS-NR                    OCCURS 040        
                                                      PIC  9(004).      
                 40 SA421-PROZENTSATZ                 OCCURS 040        
                                                      PIC  9(003)V9(02).
                                                                        
              35 SA422-PORTFOLIO-ERWEITERUNG                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA422-NUMMER                      PIC  9(007).      
                 40 SA422-FONDS-NR                    OCCURS 039        
                                                      PIC  9(004).      
                 40 SA422-PROZENTSATZ                 OCCURS 039        
                                                      PIC  9(003)V9(02).
                 40 FILLER                            PIC  X(017).      
                                                                        
              35 SA700-TAGESPREISE-DER-FONDS                            
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA700-PREISTAG                    PIC  9(008).      
                 40 SA700-AUSGABEPREIS                PIC  9(007)V9(06).
                 40 SA700-RUECKNAHMEPREIS             PIC  9(007)V9(06).
                 40 SA700-AUSGABE-AUFSCHLAG           PIC  9(003)V9(02).
                 40 SA700-NAV                         PIC  9(007)V9(06).
                 40 SA700-AKTIENGEWINN-NEGATIV        PIC  X(001).      
                 40 SA700-AKTIENGEWINN                PIC  9(007)V9(06).
                 40 SA700-ZWIST-EKST                  PIC  9(007)V9(06).
                 40 SA700-ZWIST-ZAST                  PIC  9(007)V9(06).
                 40 SA700-EU-ZWIG                     PIC  9(007)V9(06).
                 40 SA700-EU-ZWIG-CH                  PIC  9(007)V9(06).
                 40 SA700-EU-ZWIG-GEMELD              PIC  9(001).      
                 40 SA700-EU-RALAVANZ                 PIC  9(001).      
                 40 SA700-EU-FORDANTEIL-VK            PIC  9(001).      
                                                                        
              35 SA710-KAG-STAMM                                        
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA710-KAG-NR                      PIC  9(004).      
                 40 SA710-KAG                         PIC  X(030).      
                 40 SA710-INSTITUT                    PIC  X(030).      
                 40 SA710-FILIALE                     PIC  X(030).      
                 40 SA710-STRASSE                     PIC  X(030).      
                 40 SA710-PLZ                         PIC  X(007).      
                 40 SA710-ORT                         PIC  X(030).      
                 40 SA710-ZUSATZTEXT                  PIC  X(030).      
                 40 SA710-LAND                        PIC  9(003).      
                                                                        
              35 SA800-TABELLEN                                         
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA800-TABELLEN-NR                 PIC  9(003).      
                 40 SA800-TABELLEN-SCHLUESSEL         PIC  X(004).      
                 40 SA800-SCHLUESSEL-BEZEICHNUNG      PIC  X(030).      
                 40 SA800-FONDS-WKN                   PIC  X(012).      
                 40 SA800-FONDS-WAEHRUNG              PIC  X(003).      
                 40 SA800-FONDS-KZ                    PIC  9(001).      
                 40 SA800-FONDS-KURZBEZ               PIC  X(015).      
                 40 SA800-FONDS-RISIKOKLASSE          PIC  9(001).      
                 40 SA800-UMRECH-IN-KONSWAEHRUNG      PIC  9(006)V9(05).
                 40 SA800-KURSBASIS-ZUR-WAEHRUNG      PIC  9(007).      
                 40 SA800-KURSTAG-DER-WAEHRUNG        PIC  9(008).      
                 40 SA800-PROVISION-INDEX             PIC  9(002).      
                 40 SA800-PROVSION-UMSATZSCHL         PIC  9(002).      
                 40 SA800-PROVISION-LAUFZEIT          PIC  9(002).      
                 40 SA800-PROVISION-BETRAG-VON        PIC  9(013)V9(04).
                 40 SA800-PROVISION-BETRAG-BIS        PIC  9(013)V9(04).
                 40 SA800-PROVISION-KONSORTE          PIC  9(003)V9(02).
                 40 SA800-PROVISION-INV-KTO           PIC  9(003)V9(02).
                 40 SA800-ISIN                        PIC  X(012).      
                 40 SA800-AUSGABE-AUFSCHLAG           PIC  9(003)V9(02).
                 40 SA800-FONDS-BERECHNUNGSMETH       PIC  9(002).      
                 40 SA800-VERWALTUNGSVERGUETUNG       PIC  9(003)V9(04).
                 40 SA800-BASISPUNKTE                 PIC  9(003)V9(04).
                 40 SA800-RUECKNAHMEGEBUEHR-PROZ      PIC  9(003)V9(04).
                 40 SA800-KAG-NR                      PIC  9(004).      
                 40 SA800-LAND-KZ                     PIC  X(003).      
                                                                        
              35 SA999-KONTROLLSATZ                                     
                    REDEFINES KS025-SPEZIELLER-TEIL.                    
                 40 SA999-ANZAHL-SA020                PIC  9(009).      
                 40 SA999-ANZAHL-SA030                PIC  9(009).      
                 40 SA999-ANZAHL-SA040                PIC  9(009).      
                 40 SA999-ANZAHL-SA048                PIC  9(009).      
                 40 SA999-ANZAHL-SA700                PIC  9(009).      
                 40 SA999-ANZAHL-SA800                PIC  9(009).      
                 40 SA999-ANZAHL-SA108                PIC  9(009).      
                 40 SA999-ANZAHL-SA120                PIC  9(009).      
                 40 SA999-ANZAHL-SA130                PIC  9(009).      
                 40 SA999-ANZAHL-SA045                PIC  9(009).      
                 40 SA999-ANZAHL-SA021                PIC  9(009).      
                 40 SA999-ANZAHL-SA400-401            PIC  9(009).      
                 40 SA999-ANZAHL-SA402                PIC  9(009).      
                 40 SA999-ANZAHL-SA404                PIC  9(009).      
                 40 SA999-ANZAHL-SA022-023            PIC  9(009).      
                 40 SA999-ANZAHL-SA111-112-113-114    PIC  9(009).      
                 40 SA999-ANZAHL-SA420-421-422        PIC  9(009).      
                 40 SA999-ANZAHL-SA405                PIC  9(009).      
                 40 SA999-ANZAHL-SA024                PIC  9(009).      
                 40 SA999-ANZAHL-SA710                PIC  9(009).      
                 40 SA999-ANZAHL-SA300                PIC  9(009).      
                 40 SA999-ANZAHL-SA301                PIC  9(009).      
                 40 SA999-ANZAHL-SA406                PIC  9(009).      
                 40 SA999-ANZAHL-SA027                PIC  9(009).      
                 40 SA999-ANZAHL-SA041                PIC  9(009).      
                 40 SA999-ANZAHL-SA042                PIC  9(009).      
                 40 SA999-ANZAHL-SA194                PIC  9(009).      
                 40 SA999-ANZAHL-SA195                PIC  9(009).      
                 40 SA999-ANZAHL-SA196                PIC  9(009).      
                 40 SA999-ANZAHL-SA197                PIC  9(009).      
                 40 SA999-ANZAHL-SA198                PIC  9(009).      
                 40 SA999-ANZAHL-SA199                PIC  9(009).      
                 40 SA999-ANZAHL-SA031                PIC  9(009).      
                 40 SA999-ANZAHL-SA200                PIC  9(009).      
                 40 SA999-ANZAHL-SA201                PIC  9(009).      
                 40 SA999-ANZAHL-SA202                PIC  9(009).      
                 40 SA999-ANZAHL-SA026                PIC  9(009).      
                 40 SA999-ANZAHL-SA028                PIC  9(009).      
      *                                                                 
      ******** ENDE COPY-MEMBER **************************************  
