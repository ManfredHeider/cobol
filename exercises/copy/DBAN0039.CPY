      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0039.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/DBAN0039.CPv  $
      *
      *   Rev 2.15   Jul 25 2013 10:49:58   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.14   Aug 21 2012 15:40:34   t13ae23
      *AAN01065: Mandant-Österreich: Nutzung DVAG-OS durch VB-Stufe
      * "VBA"
      *
      *   Rev 2.13   Dec 14 2011 17:01:58   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.12   Jun 15 2010 10:03:24   t13ae23
      *AAN00926: 2. Stufe Platzhalterersetzen elektr. Antrag Ges. 1
      *6 u. 26
      *
      *   Rev 2.11   Aug 18 2009 13:56:08   t13ae23
      *AAN00909: Ges. 16+26: Antragserfassung - Wechsel Vorname - Z
      *uname
      *
      *   Rev 2.10   Aug 18 2009 10:58:12   t13ae23
      *AAN00909: Ges. 16+26: Antragserfassung - Wechsel Vorname - Z
      *uname
      *
      *   Rev 2.9   Aug 18 2009 09:58:24   t13ae23
      *AAN00909: Ges. 16+26: Antragserfassung - Wechsel Vorname - Z
      *uname
      *
      *   Rev 2.8   Aug 18 2009 09:47:40   t13ae23
      *AAN00909: Ges. 16+26: Antragserfassung - Wechsel Vorname - Z
      *uname
      *
      *   Rev 2.7   Nov 24 2008 14:19:04   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.6   Nov 24 2008 11:50:08   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.5   Jun 12 2008 16:15:38   t13ae23
      *AAN00805: Anpassung Eingabemaske für neues Prod. 53
      *Ges. 26
      *
      *   Rev 2.4   Mar 09 2007 16:42:10   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Feb 13 2007 19:31:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Feb 13 2007 18:30:54   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Feb 13 2007 17:23:48   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   27 Dec 2006 11:16:16   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MAINFRAME EXPRESS 3.1  6.012.0031
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-54)
      *   MAPSET NAME   BAN0039
      *   DATE CREATED  05/21/2013
      *   TIME CREATED  10:23:43

      *  INPUT DATA FOR MAP BAN0039
         01 BAN0039I.
            03 FILLER                         PIC X(12).
            03 TVBNRL                         PIC S9(4) COMP.
            03 TVBNRF                         PIC X.
            03 FILLER REDEFINES TVBNRF.
               05 TVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVBNRI                         PIC X(11).
            03 AVBNRL                         PIC S9(4) COMP.
            03 AVBNRF                         PIC X.
            03 FILLER REDEFINES AVBNRF.
               05 AVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNRI                         PIC X(7).
            03 AVBPRZL                        PIC S9(4) COMP.
            03 AVBPRZF                        PIC X.
            03 FILLER REDEFINES AVBPRZF.
               05 AVBPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVBPRZI                        PIC X(1).
            03 ADIRL                          PIC S9(4) COMP.
            03 ADIRF                          PIC X.
            03 FILLER REDEFINES ADIRF.
               05 ADIRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 ADIRI                          PIC X(3).
            03 AVBNAMEL                       PIC S9(4) COMP.
            03 AVBNAMEF                       PIC X.
            03 FILLER REDEFINES AVBNAMEF.
               05 AVBNAMEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNAMEI                       PIC X(20).
            03 TVORSCL                        PIC S9(4) COMP.
            03 TVORSCF                        PIC X.
            03 FILLER REDEFINES TVORSCF.
               05 TVORSCA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVORSCI                        PIC X(8).
            03 AVORSCL                        PIC S9(4) COMP.
            03 AVORSCF                        PIC X.
            03 FILLER REDEFINES AVORSCF.
               05 AVORSCA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVORSCI                        PIC X(16).
            03 TVMNRL                         PIC S9(4) COMP.
            03 TVMNRF                         PIC X.
            03 FILLER REDEFINES TVMNRF.
               05 TVMNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVMNRI                         PIC X(11).
            03 AVMNRL                         PIC S9(4) COMP.
            03 AVMNRF                         PIC X.
            03 FILLER REDEFINES AVMNRF.
               05 AVMNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNRI                         PIC X(7).
            03 AVMPRZL                        PIC S9(4) COMP.
            03 AVMPRZF                        PIC X.
            03 FILLER REDEFINES AVMPRZF.
               05 AVMPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVMPRZI                        PIC X(1).
            03 AVMNAMEL                       PIC S9(4) COMP.
            03 AVMNAMEF                       PIC X.
            03 FILLER REDEFINES AVMNAMEF.
               05 AVMNAMEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNAMEI                       PIC X(20).
            03 TVBGWL                         PIC S9(4) COMP.
            03 TVBGWF                         PIC X.
            03 FILLER REDEFINES TVBGWF.
               05 TVBGWA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVBGWI                         PIC X(13).
            03 AVBGWL                         PIC S9(4) COMP.
            03 AVBGWF                         PIC X.
            03 FILLER REDEFINES AVBGWF.
               05 AVBGWA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVBGWI                         PIC X(7).
            03 TPINL                          PIC S9(4) COMP.
            03 TPINF                          PIC X.
            03 FILLER REDEFINES TPINF.
               05 TPINA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TPINI                          PIC X(10).
            03 EPINL                          PIC S9(4) COMP.
            03 EPINF                          PIC X.
            03 FILLER REDEFINES EPINF.
               05 EPINA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPINI                          PIC X(7).
            03 EPINPRL                        PIC S9(4) COMP.
            03 EPINPRF                        PIC X.
            03 FILLER REDEFINES EPINPRF.
               05 EPINPRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPINPRI                        PIC X(2).
            03 TVSNRL                         PIC S9(4) COMP.
            03 TVSNRF                         PIC X.
            03 FILLER REDEFINES TVSNRF.
               05 TVSNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVSNRI                         PIC X(7).
            03 AVSNRL                         PIC S9(4) COMP.
            03 AVSNRF                         PIC X.
            03 FILLER REDEFINES AVSNRF.
               05 AVSNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVSNRI                         PIC X(15).
            03 AVBZNAML                       PIC S9(4) COMP.
            03 AVBZNAMF                       PIC X.
            03 FILLER REDEFINES AVBZNAMF.
               05 AVBZNAMA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBZNAMI                       PIC X(20).
            03 TKDTITL                        PIC S9(4) COMP.
            03 TKDTITF                        PIC X.
            03 FILLER REDEFINES TKDTITF.
               05 TKDTITA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDTITI                        PIC X(11).
            03 EKDTITL                        PIC S9(4) COMP.
            03 EKDTITF                        PIC X.
            03 FILLER REDEFINES EKDTITF.
               05 EKDTITA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDTITI                        PIC X(26).
            03 TKDNAML                        PIC S9(4) COMP.
            03 TKDNAMF                        PIC X.
            03 FILLER REDEFINES TKDNAMF.
               05 TKDNAMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDNAMI                        PIC X(10).
            03 EKDNAML                        PIC S9(4) COMP.
            03 EKDNAMF                        PIC X.
            03 FILLER REDEFINES EKDNAMF.
               05 EKDNAMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDNAMI                        PIC X(26).
            03 TKDVNAL                        PIC S9(4) COMP.
            03 TKDVNAF                        PIC X.
            03 FILLER REDEFINES TKDVNAF.
               05 TKDVNAA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDVNAI                        PIC X(7).
            03 EKDVNAL                        PIC S9(4) COMP.
            03 EKDVNAF                        PIC X.
            03 FILLER REDEFINES EKDVNAF.
               05 EKDVNAA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDVNAI                        PIC X(26).
            03 TPLZL                          PIC S9(4) COMP.
            03 TPLZF                          PIC X.
            03 FILLER REDEFINES TPLZF.
               05 TPLZA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TPLZI                          PIC X(10).
            03 EAUSLL                         PIC S9(4) COMP.
            03 EAUSLF                         PIC X.
            03 FILLER REDEFINES EAUSLF.
               05 EAUSLA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EAUSLI                         PIC X(3).
            03 EPLZL                          PIC S9(4) COMP.
            03 EPLZF                          PIC X.
            03 FILLER REDEFINES EPLZF.
               05 EPLZA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPLZI                          PIC X(5).
            03 EKDORTL                        PIC S9(4) COMP.
            03 EKDORTF                        PIC X.
            03 FILLER REDEFINES EKDORTF.
               05 EKDORTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDORTI                        PIC X(26).
            03 TKDSTRL                        PIC S9(4) COMP.
            03 TKDSTRF                        PIC X.
            03 FILLER REDEFINES TKDSTRF.
               05 TKDSTRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDSTRI                        PIC X(7).
            03 EKDSTRL                        PIC S9(4) COMP.
            03 EKDSTRF                        PIC X.
            03 FILLER REDEFINES EKDSTRF.
               05 EKDSTRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDSTRI                        PIC X(26).
            03 TKDSVNRL                       PIC S9(4) COMP.
            03 TKDSVNRF                       PIC X.
            03 FILLER REDEFINES TKDSVNRF.
               05 TKDSVNRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDSVNRI                       PIC X(11).
            03 EKDSVNRL                       PIC S9(4) COMP.
            03 EKDSVNRF                       PIC X.
            03 FILLER REDEFINES EKDSVNRF.
               05 EKDSVNRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDSVNRI                       PIC X(4).
            03 TKDSTAAL                       PIC S9(4) COMP.
            03 TKDSTAAF                       PIC X.
            03 FILLER REDEFINES TKDSTAAF.
               05 TKDSTAAA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDSTAAI                       PIC X(10).
            03 EKDSTAAL                       PIC S9(4) COMP.
            03 EKDSTAAF                       PIC X.
            03 FILLER REDEFINES EKDSTAAF.
               05 EKDSTAAA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDSTAAI                       PIC X(3).
            03 TKDBGRPL                       PIC S9(4) COMP.
            03 TKDBGRPF                       PIC X.
            03 FILLER REDEFINES TKDBGRPF.
               05 TKDBGRPA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDBGRPI                       PIC X(10).
            03 EKDBGRPL                       PIC S9(4) COMP.
            03 EKDBGRPF                       PIC X.
            03 FILLER REDEFINES EKDBGRPF.
               05 EKDBGRPA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDBGRPI                       PIC X(40).
            03 TKDPEPL                        PIC S9(4) COMP.
            03 TKDPEPF                        PIC X.
            03 FILLER REDEFINES TKDPEPF.
               05 TKDPEPA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDPEPI                        PIC X(10).
            03 EKDPEPL                        PIC S9(4) COMP.
            03 EKDPEPF                        PIC X.
            03 FILLER REDEFINES EKDPEPF.
               05 EKDPEPA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDPEPI                        PIC X(1).
            03 TKDBRANL                       PIC S9(4) COMP.
            03 TKDBRANF                       PIC X.
            03 FILLER REDEFINES TKDBRANF.
               05 TKDBRANA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDBRANI                       PIC X(7).
            03 EKDBRANL                       PIC S9(4) COMP.
            03 EKDBRANF                       PIC X.
            03 FILLER REDEFINES EKDBRANF.
               05 EKDBRANA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDBRANI                       PIC X(40).
            03 TTARIFL                        PIC S9(4) COMP.
            03 TTARIFF                        PIC X.
            03 FILLER REDEFINES TTARIFF.
               05 TTARIFA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TTARIFI                        PIC X(25).
            03 ETARIFL                        PIC S9(4) COMP.
            03 ETARIFF                        PIC X.
            03 FILLER REDEFINES ETARIFF.
               05 ETARIFA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ETARIFI                        PIC X(15).
            03 TFILL01L                       PIC S9(4) COMP.
            03 TFILL01F                       PIC X.
            03 FILLER REDEFINES TFILL01F.
               05 TFILL01A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TFILL01I                       PIC X(1).
            03 EPRODL                         PIC S9(4) COMP.
            03 EPRODF                         PIC X.
            03 FILLER REDEFINES EPRODF.
               05 EPRODA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPRODI                         PIC X(2).
            03 EGEBURL                        PIC S9(4) COMP.
            03 EGEBURF                        PIC X.
            03 FILLER REDEFINES EGEBURF.
               05 EGEBURA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EGEBURI                        PIC X(6).
            03 ESUMMEL                        PIC S9(4) COMP.
            03 ESUMMEF                        PIC X.
            03 FILLER REDEFINES ESUMMEF.
               05 ESUMMEA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESUMMEI                        PIC X(9).
            03 TVEBEGL                        PIC S9(4) COMP.
            03 TVEBEGF                        PIC X.
            03 FILLER REDEFINES TVEBEGF.
               05 TVEBEGA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVEBEGI                        PIC X(20).
            03 EVEBEGL                        PIC S9(4) COMP.
            03 EVEBEGF                        PIC X.
            03 FILLER REDEFINES EVEBEGF.
               05 EVEBEGA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVEBEGI                        PIC X(6).
            03 EBZDAUL                        PIC S9(4) COMP.
            03 EBZDAUF                        PIC X.
            03 FILLER REDEFINES EBZDAUF.
               05 EBZDAUA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBZDAUI                        PIC X(2).
            03 TDYNAJL                        PIC S9(4) COMP.
            03 TDYNAJF                        PIC X.
            03 FILLER REDEFINES TDYNAJF.
               05 TDYNAJA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TDYNAJI                        PIC X(20).
            03 EDYNAJL                        PIC S9(4) COMP.
            03 EDYNAJF                        PIC X.
            03 FILLER REDEFINES EDYNAJF.
               05 EDYNAJA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EDYNAJI                        PIC X(1).
            03 EBUZBJL                        PIC S9(4) COMP.
            03 EBUZBJF                        PIC X.
            03 FILLER REDEFINES EBUZBJF.
               05 EBUZBJA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBUZBJI                        PIC X(1).
            03 EBUZRJL                        PIC S9(4) COMP.
            03 EBUZRJF                        PIC X.
            03 FILLER REDEFINES EBUZRJF.
               05 EBUZRJA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBUZRJI                        PIC X(1).
            03 EPOJL                          PIC S9(4) COMP.
            03 EPOJF                          PIC X.
            03 FILLER REDEFINES EPOJF.
               05 EPOJA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPOJI                          PIC X(1).
            03 EUZVEJL                        PIC S9(4) COMP.
            03 EUZVEJF                        PIC X.
            03 FILLER REDEFINES EUZVEJF.
               05 EUZVEJA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EUZVEJI                        PIC X(1).
            03 TBEITRL                        PIC S9(4) COMP.
            03 TBEITRF                        PIC X.
            03 FILLER REDEFINES TBEITRF.
               05 TBEITRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TBEITRI                        PIC X(20).
            03 EBEITRL                        PIC S9(4) COMP.
            03 EBEITRF                        PIC X.
            03 FILLER REDEFINES EBEITRF.
               05 EBEITRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBEITRI                        PIC X(13).
            03 EZAHLWL                        PIC S9(4) COMP.
            03 EZAHLWF                        PIC X.
            03 FILLER REDEFINES EZAHLWF.
               05 EZAHLWA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EZAHLWI                        PIC X(1).
            03 TSUMME2L                       PIC S9(4) COMP.
            03 TSUMME2F                       PIC X.
            03 FILLER REDEFINES TSUMME2F.
               05 TSUMME2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TSUMME2I                       PIC X(19).
            03 EBETR2L                        PIC S9(4) COMP.
            03 EBETR2F                        PIC X.
            03 FILLER REDEFINES EBETR2F.
               05 EBETR2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBETR2I                        PIC X(13).
            03 TBEITR2L                       PIC S9(4) COMP.
            03 TBEITR2F                       PIC X.
            03 FILLER REDEFINES TBEITR2F.
               05 TBEITR2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TBEITR2I                       PIC X(20).
            03 EBETR1L                        PIC S9(4) COMP.
            03 EBETR1F                        PIC X.
            03 FILLER REDEFINES EBETR1F.
               05 EBETR1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBETR1I                        PIC X(13).
            03 TEINHL                         PIC S9(4) COMP.
            03 TEINHF                         PIC X.
            03 FILLER REDEFINES TEINHF.
               05 TEINHA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TEINHI                         PIC X(9).
            03 AEINHL                         PIC S9(4) COMP.
            03 AEINHF                         PIC X.
            03 FILLER REDEFINES AEINHF.
               05 AEINHA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AEINHI                         PIC X(11).
            03 EEINHL                         PIC S9(4) COMP.
            03 EEINHF                         PIC X.
            03 FILLER REDEFINES EEINHF.
               05 EEINHA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EEINHI                         PIC X(11).
            03 TKNRBLZL                       PIC S9(4) COMP.
            03 TKNRBLZF                       PIC X.
            03 FILLER REDEFINES TKNRBLZF.
               05 TKNRBLZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKNRBLZI                       PIC X(20).
            03 EKDKNRL                        PIC S9(4) COMP.
            03 EKDKNRF                        PIC X.
            03 FILLER REDEFINES EKDKNRF.
               05 EKDKNRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDKNRI                        PIC X(15).
            03 EKDBLZL                        PIC S9(4) COMP.
            03 EKDBLZF                        PIC X.
            03 FILLER REDEFINES EKDBLZF.
               05 EKDBLZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDBLZI                        PIC X(15).
            03 TIBANL                         PIC S9(4) COMP.
            03 TIBANF                         PIC X.
            03 FILLER REDEFINES TIBANF.
               05 TIBANA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TIBANI                         PIC X(4).
            03 EIBANL                         PIC S9(4) COMP.
            03 EIBANF                         PIC X.
            03 FILLER REDEFINES EIBANF.
               05 EIBANA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EIBANI                         PIC X(34).
            03 TANARTL                        PIC S9(4) COMP.
            03 TANARTF                        PIC X.
            03 FILLER REDEFINES TANARTF.
               05 TANARTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TANARTI                        PIC X(7).
            03 EANARTL                        PIC S9(4) COMP.
            03 EANARTF                        PIC X.
            03 FILLER REDEFINES EANARTF.
               05 EANARTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EANARTI                        PIC X(1).
            03 TAUFNDL                        PIC S9(4) COMP.
            03 TAUFNDF                        PIC X.
            03 FILLER REDEFINES TAUFNDF.
               05 TAUFNDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TAUFNDI                        PIC X(6).
            03 EAUFNDL                        PIC S9(4) COMP.
            03 EAUFNDF                        PIC X.
            03 FILLER REDEFINES EAUFNDF.
               05 EAUFNDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EAUFNDI                        PIC X(6).
            03 TPESTRL                        PIC S9(4) COMP.
            03 TPESTRF                        PIC X.
            03 FILLER REDEFINES TPESTRF.
               05 TPESTRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TPESTRI                        PIC X(6).
            03 EPESTRL                        PIC S9(4) COMP.
            03 EPESTRF                        PIC X.
            03 FILLER REDEFINES EPESTRF.
               05 EPESTRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPESTRI                        PIC X(6).
            03 TSTATIL                        PIC S9(4) COMP.
            03 TSTATIF                        PIC X.
            03 FILLER REDEFINES TSTATIF.
               05 TSTATIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TSTATII                        PIC X(6).
            03 ESTATIL                        PIC S9(4) COMP.
            03 ESTATIF                        PIC X.
            03 FILLER REDEFINES ESTATIF.
               05 ESTATIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESTATII                        PIC X(6).
            03 TSTATUL                        PIC S9(4) COMP.
            03 TSTATUF                        PIC X.
            03 FILLER REDEFINES TSTATUF.
               05 TSTATUA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TSTATUI                        PIC X(6).
            03 ESTATUL                        PIC S9(4) COMP.
            03 ESTATUF                        PIC X.
            03 FILLER REDEFINES ESTATUF.
               05 ESTATUA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESTATUI                        PIC X(2).
            03 ASTATUL                        PIC S9(4) COMP.
            03 ASTATUF                        PIC X.
            03 FILLER REDEFINES ASTATUF.
               05 ASTATUA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASTATUI                        PIC X(12).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER REDEFINES AFCZANZF.
               05 AFCZANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BAN0039
         01 BAN0039O REDEFINES BAN0039I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TVBNRH                         PIC X.
            03 TVBNRO                         PIC X(11).
            03 FILLER                         PIC X(3).
            03 AVBNRH                         PIC X.
            03 AVBNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVBPRZH                        PIC X.
            03 AVBPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADIRH                          PIC X.
            03 ADIRO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 AVBNAMEH                       PIC X.
            03 AVBNAMEO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TVORSCH                        PIC X.
            03 TVORSCO                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 AVORSCH                        PIC X.
            03 AVORSCO                        PIC X(16).
            03 FILLER                         PIC X(3).
            03 TVMNRH                         PIC X.
            03 TVMNRO                         PIC X(11).
            03 FILLER                         PIC X(3).
            03 AVMNRH                         PIC X.
            03 AVMNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVMPRZH                        PIC X.
            03 AVMPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AVMNAMEH                       PIC X.
            03 AVMNAMEO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TVBGWH                         PIC X.
            03 TVBGWO                         PIC X(13).
            03 FILLER                         PIC X(3).
            03 AVBGWH                         PIC X.
            03 AVBGWO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 TPINH                          PIC X.
            03 TPINO                          PIC X(10).
            03 FILLER                         PIC X(3).
            03 EPINH                          PIC X.
            03 EPINO                          PIC X(7).
            03 FILLER                         PIC X(3).
            03 EPINPRH                        PIC X.
            03 EPINPRO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TVSNRH                         PIC X.
            03 TVSNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNRH                         PIC X.
            03 AVSNRO                         PIC X(15).
            03 FILLER                         PIC X(3).
            03 AVBZNAMH                       PIC X.
            03 AVBZNAMO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TKDTITH                        PIC X.
            03 TKDTITO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 EKDTITH                        PIC X.
            03 EKDTITO                        PIC X(26).
            03 FILLER                         PIC X(3).
            03 TKDNAMH                        PIC X.
            03 TKDNAMO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKDNAMH                        PIC X.
            03 EKDNAMO                        PIC X(26).
            03 FILLER                         PIC X(3).
            03 TKDVNAH                        PIC X.
            03 TKDVNAO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EKDVNAH                        PIC X.
            03 EKDVNAO                        PIC X(26).
            03 FILLER                         PIC X(3).
            03 TPLZH                          PIC X.
            03 TPLZO                          PIC X(10).
            03 FILLER                         PIC X(3).
            03 EAUSLH                         PIC X.
            03 EAUSLO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 EPLZH                          PIC X.
            03 EPLZO                          PIC X(5).
            03 FILLER                         PIC X(3).
            03 EKDORTH                        PIC X.
            03 EKDORTO                        PIC X(26).
            03 FILLER                         PIC X(3).
            03 TKDSTRH                        PIC X.
            03 TKDSTRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EKDSTRH                        PIC X.
            03 EKDSTRO                        PIC X(26).
            03 FILLER                         PIC X(3).
            03 TKDSVNRH                       PIC X.
            03 TKDSVNRO                       PIC X(11).
            03 FILLER                         PIC X(3).
            03 EKDSVNRH                       PIC X.
            03 EKDSVNRO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 TKDSTAAH                       PIC X.
            03 TKDSTAAO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKDSTAAH                       PIC X.
            03 EKDSTAAO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 TKDBGRPH                       PIC X.
            03 TKDBGRPO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKDBGRPH                       PIC X.
            03 EKDBGRPO                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 TKDPEPH                        PIC X.
            03 TKDPEPO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKDPEPH                        PIC X.
            03 EKDPEPO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TKDBRANH                       PIC X.
            03 TKDBRANO                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 EKDBRANH                       PIC X.
            03 EKDBRANO                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 TTARIFH                        PIC X.
            03 TTARIFO                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 ETARIFH                        PIC X.
            03 ETARIFO                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 TFILL01H                       PIC X.
            03 TFILL01O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EPRODH                         PIC X.
            03 EPRODO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 EGEBURH                        PIC X.
            03 EGEBURO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 ESUMMEH                        PIC X.
            03 ESUMMEO                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 TVEBEGH                        PIC X.
            03 TVEBEGO                        PIC X(20).
            03 FILLER                         PIC X(3).
            03 EVEBEGH                        PIC X.
            03 EVEBEGO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EBZDAUH                        PIC X.
            03 EBZDAUO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TDYNAJH                        PIC X.
            03 TDYNAJO                        PIC X(20).
            03 FILLER                         PIC X(3).
            03 EDYNAJH                        PIC X.
            03 EDYNAJO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBUZBJH                        PIC X.
            03 EBUZBJO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBUZRJH                        PIC X.
            03 EBUZRJO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EPOJH                          PIC X.
            03 EPOJO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 EUZVEJH                        PIC X.
            03 EUZVEJO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TBEITRH                        PIC X.
            03 TBEITRO                        PIC X(20).
            03 FILLER                         PIC X(3).
            03 EBEITRH                        PIC X.
            03 EBEITRO                        PIC X(13).
            03 FILLER                         PIC X(3).
            03 EZAHLWH                        PIC X.
            03 EZAHLWO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TSUMME2H                       PIC X.
            03 TSUMME2O                       PIC X(19).
            03 FILLER                         PIC X(3).
            03 EBETR2H                        PIC X.
            03 EBETR2O                        PIC X(13).
            03 FILLER                         PIC X(3).
            03 TBEITR2H                       PIC X.
            03 TBEITR2O                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 EBETR1H                        PIC X.
            03 EBETR1O                        PIC X(13).
            03 FILLER                         PIC X(3).
            03 TEINHH                         PIC X.
            03 TEINHO                         PIC X(9).
            03 FILLER                         PIC X(3).
            03 AEINHH                         PIC X.
            03 AEINHO                         PIC X(11).
            03 FILLER                         PIC X(3).
            03 EEINHH                         PIC X.
            03 EEINHO                         PIC X(11).
            03 FILLER                         PIC X(3).
            03 TKNRBLZH                       PIC X.
            03 TKNRBLZO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 EKDKNRH                        PIC X.
            03 EKDKNRO                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 EKDBLZH                        PIC X.
            03 EKDBLZO                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 TIBANH                         PIC X.
            03 TIBANO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 EIBANH                         PIC X.
            03 EIBANO                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 TANARTH                        PIC X.
            03 TANARTO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EANARTH                        PIC X.
            03 EANARTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TAUFNDH                        PIC X.
            03 TAUFNDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EAUFNDH                        PIC X.
            03 EAUFNDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TPESTRH                        PIC X.
            03 TPESTRO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EPESTRH                        PIC X.
            03 EPESTRO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TSTATIH                        PIC X.
            03 TSTATIO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 ESTATIH                        PIC X.
            03 ESTATIO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TSTATUH                        PIC X.
            03 TSTATUO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 ESTATUH                        PIC X.
            03 ESTATUO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ASTATUH                        PIC X.
            03 ASTATUO                        PIC X(12).
            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

