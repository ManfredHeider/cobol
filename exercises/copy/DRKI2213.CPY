      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  22.11.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2213.CPY  $                                 00010000










                                                                        
                                                                        
      *    $Revision:   2.12  $                                         
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2213.CPv  $                            
      *
      *   Rev 2.12   Jun 10 2014 14:47:00   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.11   Jun 10 2014 12:10:48   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.10   Jun 05 2014 13:59:16   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.9   Jun 05 2014 13:52:00   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.8   Jun 04 2014 08:50:36   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.7   Feb 26 2014 16:54:50   t13ex15
      *FKI01447: AGI Bestands-DTA Ergänzung ISIN
      *
      *   Rev 2.6   Jun 17 2009 14:28:38   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.5   Jun 15 2009 16:33:48   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.4   Dec 22 2008 11:18:28   t13ex50
      *AKI00760: AGI/FodB (ProdPartner 40): Anpassung von Buchungss
      *chlüsseln
      *
      *   Rev 2.3   Jan 14 2008 12:06:48   t13ex50
      *AKI00693: Masch. sichern + Verarb. AGI Komplett- und Tagesbe
      *stände 
      *                                                                 
      *   Rev 2.2   Jan 14 2008 09:48:22   t13ex50                      
      *AKI00693: Korrektur Akt. Bestände (Buchungsschlüssel)            
      *                                                                 
      *   Rev 2.1   Dec 19 2007 13:55:58   t13ex50                      
      *AKI00639: PremiumDepot: Datenspiegel und Programmanpassungen     
      *                                                                 
      *   Rev 2.0   Nov 22 2007 09:59:24   t13ex50                      
      *Archiverstellung                                                 
      *                                                                 
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 2 1 3    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME von agi,              *
      *    AUFBEREITUNG DER BESTANDSVERAENDERUNGEN.                    *
      *        - AUSGABE: BESTANDSVERAENDERUNGEN                       *
      *                                                                *
      *    SATZLÄNGE 413                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *22.11.07!NEUERSTELLUNG KOPIE DES DRKI1321 +     !SCHLEICHER 2.0 *
      *AKI00639! 3 FELDER FÜR AGI + FILLER VERGRÖßERT  !         !     *
      *        !ACHTUNG: BUCHUNGSSCHLÜSSEL HAT JETZT   !         !     *
      *        !         3 ANSTATT 2 STELLEN           !         !     *
      *--------!---------------------------------------!---------!-----*
140108*14.01.08!KORREKTUR BUCHUNGSSCHLÜSSEL VORZEICHEN !SCHLEICHER 2.2 *
      *AKI00693!   WIEDER EINGEFÜGT.                   !         !     *
      *--------!---------------------------------------!---------!-----*
140108*14.01.08!FILLER UM 1 ERHÖHT UND AUF 300 STELLEN !SCHLEICHER 2.3 *
      *AKI00693!   ZU KOMMEN                           !         !     *
      *--------!---------------------------------------!---------!-----*
171208*17.12.08!BUCHUNGSSCHLÜSSEL 1 STELLE VERLÄNGERT  !SCHLEICHER 2.4 *
      *AKI00760!  FILLER 1 STELLE VERKÜRZT             !         !     *
      *--------!---------------------------------------!---------!-----*
160109*16.01.09!NEUE AUFBAU PRIMARYKEY                 !WHO      !2.5 *
      *--------!---------------------------------------!---------!-----*
      *18.02.14!Umstellung wegen zusätzlicher ISIN     !SWZ      !2.7  *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
260514*26.05.14!Felder raus genommen                   !SWZ      !2.08 *
      ******************************************************************
       01  DRKI2213.                                                    
           05  BA-VERT-ID                        PIC  X(13).
260514*    Die Felder sollen künftig nur noch in DRKI2212 vorkommen,
260514*    im PGM wird BA-Gesamt mit DRKI2212 redefiniert
           05  BA-GESAMT                         PIC  X(400).

