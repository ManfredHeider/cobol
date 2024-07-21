      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.10.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2433.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2433.CPv  $
      *
      *   Rev 2.4   Jan 03 2011 14:21:34   t13ex52
      *FKI01320: Fehlerbehebung bei Übertrag Buchungstextschlüssel 
      *in interne Tabelle
      *
      *   Rev 2.3   Nov 11 2010 19:05:44   t13ex52
      *AKI00968: BADENIA Überarbeitung aller Buchungstextschlüssel
      *
      *   Rev 2.2   Nov 11 2010 18:56:44   t13ex52
      *AKI00968: BADENIA Überarbeitung aller Buchungstextschlüssel
      *
      *   Rev 2.1   Nov 11 2010 18:44:26   t13ex52
      *AKI00968: BADENIA Überarbeitung aller Buchungstextschlüssel
      *
      *   Rev 2.0   Oct 08 2010 10:54:20   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    SUPPORT                                                      
      *    COPY-MEMBER FÜR DAS BADENIA AUFBEREITUNGSPROGRAMM FUER VER-  
      *    TRAGS- und UMSATZDATEN                                       
      *                                                                 
      *    IN DIE TABELLE WERDEN DIE MAPPINGDATEN FÜR:                  
      *    - DIE ZUORDNUNG BADENIA Buchungstextschluessel zu DVAG       
      *      Buchungstextschluessel.                                    
      *    INTEGRIERT.                                                  
      ******************************************************************
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME        
      *----------------------------------------------------------------*
      * AKI00968!01.09.10!1.0 ! NEUERSTELLUNG             !Schlenther  *
      *---------!--------!----!---------------------------!------------*
      * FKI01320!03.01.11!1.1 ! Fehlerbehebung bei Ueber- ! Schlenther *
      *    UE003!        !    ! nahme Buchungstextkey in  !            *
      *         !        !    ! die interne Tabelle       !            *
      *---------!--------!----!---------------------------!------------*
      *         !        !    !                           !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
        01  B-TEXT-SCHL-TAB.
          05  B-TEXT-SCHL-OCC       OCCURS  500.
030111***   10  B-TXT-DVAG-KEY      PIC S9(03).
030111      10  B-TXT-DVAG-KEY      PIC 9(03).
            10  B-TXT-LEERZEICHEN   PIC X(02).
030111***   10  B-TXT-BADENIA-KEY   PIC S9(03).
030111      10  B-TXT-BADENIA-KEY   PIC 9(03).
      ****
        01  B-TXT-SCHLUESSEL-MAX        PIC S9(4) COMP VALUE 500.
      *
      ******************************************************************
      *      E N D E  Copy-Buch   DRKI2433  BADENIA Buchungstextmap    *
      ******************************************************************
