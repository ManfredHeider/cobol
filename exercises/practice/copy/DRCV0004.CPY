      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2001                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 2001 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRCV0004.CPY  $                                         


      *    $Revision:   2.3  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRCV0004.CPv  $                            
      *
      *   Rev 2.3   May 30 2006 15:26:36   t13ex43
      *ASL00226: PGM7 - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.2   May 15 2006 13:51:16   t13ex43
      *ASL00226: PGM3 - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.1   Jul 28 2004 11:50:34   t13ae35
      *ASL00183: CTV-Anschreiben MAP - Manageraufbauplan
      *                                                                 
      *   Rev 2.0   02 May 2001 12:03:30   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * ASL00183!16.07.04!2.1 ! Neu                       ! Hering   *  
120506* ASL00226!12.05.06!2.2 ! VB-INFOS HINZU            ! KLEEMEIER*
300506* ASL00226!30.05.06!2.3 ! VB-INFOS ENTFERNT, NEUES  ! KLEEMEIER*
300506*         !        !    ! COPY IST DRCV0006         !          *
      *                                                              *  
      ****************************************************************  
      *                                                              *  
      *            Inhalt    :  MAP-CTV-Datei                        *  
      *            Datei     :  CV001200 und CV001300                *  
      *            Member    :  DRCV0004                             *  
      *            Organisat.:  VSAM                                 *  
      *            Satzlänge :  42 Byte                              *
      *            Key       :  VBNR (1-7)                           *  
      *                                                              *  
      *--------------------------------------------------------------*  
       01  DRCV0004-SATZ.                                               
           05 DRCV0004-VBNR                     PIC 9(007).             
           05 DRCV0004-ANSPR-VOM                PIC X(010).             
           05 DRCV0004-STUFE-BEZ                PIC X(010).             
           05 DRCV0004-MAP-BETRAG               PIC 9(009)V99.          
           05 DRCV0004-DIR-AKT                  PIC 9(004).
120506**** 05 DRCV0004-VB-ANREDE                PIC X(001).
120506**** 05 DRCV0004-VB-NAME                  PIC X(060).
      *
