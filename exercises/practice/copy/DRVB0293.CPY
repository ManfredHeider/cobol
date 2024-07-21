      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  November 1995                             
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0293.CPY  $                                         

      *    $Revision:   2.4  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB0293.CPv  $                            
      *
      *   Rev 2.4   Sep 11 2014 10:58:32   t13ex71
      *AVB01322: jährliche 34c-Ermittlung um 34f erweitern - Anpass
      *ungen im UPAN0010, UU070, ZJVB0007
      *                                                                 
      *   Rev 2.3   Sep 16 2011 15:46:24   t13ae35                      
      *AVB01161: jährliche §34c-Aktion 2011 - UU070-Anpassung           
      *                                                                 
      *   Rev 2.2   Mar 13 2009 16:11:42   t13ae35                      
      *AVB01033: §34c-Aktion 2009                                       
      *                                                                 
      *   Rev 2.1   29 Jun 2007 14:23:54   t13ae35                      
      *AVB00903: jährliche §34c-Aktion 2007                             
      *                                                                 
      *   Rev 2.0   Feb 27 1999 15:58:44   P13SP20                      
      *NT-Umstellung                                                    
      *                                                                 
      *   Rev 1.1   26 Aug 1998 10:30:18   T13AE35                      
      *AAS00049 - Neuerstellung, PI/PO fuer ZJVB0007                    
      *                                                                 
      *   Rev 1.0   23 Jul 1997 12:27:30   P13SP99                      
      *Initial revision.                                                
      *                                                                 
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRVB0293                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0007                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: WALB                                             * 
      *    DATUM:    03.07.1998                                       * 
      ***************************************************************** 
      *    VERSION  1                                                   
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AAS00049!03.07.98! 01 ! NEUERSTELLUNG             ! WALB     *  
290607* AVB00903!29.06.07! 2.1! zusätzl. BDV-Mitgliedsch. ! J.Hering *  
130309* AVB01033!13.03.09! 2.2! Gesellschafts abhängig    ! J.Hering *  
160911* AVB01161!16.09.11! 2.3! zusätzl. Anlageberatung   ! J.Hering *  
020714* AVB01322!02.07.14! 2.4! Neue Felder 34c/f Aktion  ! B.Pretsch*  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      ****************************************************************  
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *  
      *    FÜR DIE FUNKTION DRVB0293                                 *  
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *  
      *            DEM ERR-ERROR-BEREICH UND                         *  
      *            DEM STANDARD-STATUS-BEREICH                       *  
      *            (JEWEILS SEPARATE COPYS)                          *  
      *                                                              *  
      ****************************************************************  
      *                                                                 
      *                                                                 
       01 PI-DRVB0293.                                                  
          05 PI-DRVB0293-PROGRAMM           PIC  X(008).                
          05 PI-DRVB0293-UFKT-CODE          PIC  9(002).                
          05 PI-DRVB0293-MA-34C-DAT-VON     PIC  X(010).                
          05 PI-DRVB0293-MA-34C-DAT-BIS     PIC  X(010).                
130309    05 PI-DRVB0293-GESELL-TYP         PIC  S9(004) BINARY.        
      *                                                                 
       01 PO-DRVB0293.                                                  
                                                                        
          05 PO-DRVB0293-RC                 PIC  X(003).                
             88 PO-DRVB0293-RC-OK                        VALUE 'OK '.   
             88 PO-DRVB0293-RC-LEER                      VALUE 'LER'.   
             88 PO-DRVB0293-RC-EOF                       VALUE 'EOF'.   
             88 PO-DRVB0293-RC-NOT-FOUND                 VALUE 'NFD'.   
                                                                        
          05 PO-DRVB0293-DATEN.                                         
             10 PO-DRVB0293-MA-ST-NR         PIC  S9(009) COMP.         
             10 PO-DRVB0293-MA-34C-DAT       PIC  X(010).               
290607       10 PO-DRVB0293-MA-BDV-MITGLIED  PIC  X(001).               
160911       10 PO-DRVB0293-MA-ANLAGE-BER    PIC  X(001).               
020714       10 PO-DRVB0293-MA-34F-DAT       PIC  X(010).               
020714       10 PO-DRVB0293-MA-34F-REG-NR    PIC  X(015).               
020714       10 PO-DRVB0293-MA-AFO           PIC  X(001).               
020714       10 PO-DRVB0293-MA-34C-ANT-SCH   PIC  X(001).               
      *                                                                 
      ***** ENDE COPY-MEMBER DRVB0293 *****************************     
