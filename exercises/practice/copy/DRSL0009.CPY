      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 1999                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1999 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRSL0009.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRSL0009.CPv  $                            
      *
      *   Rev 2.1   Nov 28 2003 17:25:20   t13ae35
      *ASL00174 - Neu
      *                                                                 
      *   Rev 2.0   Feb 11 2000 15:19:50   P13SP20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * ASL00174!16.10.03!2.1 ! Neu                       ! Hering   *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                              *  
      *            Inhalt    :  OLB-Berechnungsbogen-CTV-Datei       *  
      *            Datei     :  SL008400 und SL008500                *  
      *            Member    :  DRSL0009                             *  
      *            Organisat.:  VSAM                                 *  
      *            Satzlänge :  172 Byte                             *  
      *            Key       :  VBNR (1-7)                           *  
      *                                                              *  
      *--------------------------------------------------------------*  
       01  DRSL0009-SATZ.                                               
           05 DRSL0009-VBNR                     PIC 9(007).             
           05 DRSL0009-PZ                       PIC X(001).             
           05 DRSL0009-MONAT                    PIC 9(002).             
           05 DRSL0009-JAHR                     PIC 9(004).             
           05 DRSL0009-VORJAHR                  PIC 9(004).             
           05 DRSL0009-OLB-BETRAG               PIC 9(009)V99.          
           05 DRSL0009-STEIGERUNG               PIC 9(009)V9.           
           05 DRSL0009-STEIGERUNG-PROZ          PIC 9(005)V99.          
           05 DRSL0009-GESELLSCHAFT             PIC 9(004).             
           05 DRSL0009-NAME-VNAME               PIC X(040).             
           05 DRSL0009-STUFE-BEZ                PIC X(004).             
           05 DRSL0009-NT-GR-VORJAHR            PIC 9(009)V9.           
           05 DRSL0009-NT-GR-AKTJAHR            PIC 9(009)V9.           
           05 DRSL0009-NT-GR-VORJAHR-ND         PIC 9(009)V9.           
           05 DRSL0009-NT-GR-AKTJAHR-ND         PIC 9(009)V9.           
           05 DRSL0009-NT-GR-VORJAHR-EFF        PIC 9(009)V9.           
           05 DRSL0009-NT-GR-AKTJAHR-EFF        PIC 9(009)V9.           
           05 DRSL0009-OLB-FAKTOR               PIC 9(001)V99.          
           05 DRSL0009-BOZ-BETRAG               PIC 9(009)V99.          
           05 DRSL0009-DIR-ZPKT                 PIC 9(004).             
