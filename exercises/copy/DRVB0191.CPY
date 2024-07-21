      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  November 1995                             
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0191.CPY  $                                         

      *    $Revision:   2.7  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB0191.CPv  $                            
      *
      *   Rev 2.7   Apr 05 2011 08:59:46   t13ex43
      *AVB01132: VB56 - Einrichten einer betreuten Direktion
      *
      *   Rev 2.6   Sep 01 2003 11:15:58   t13ae34
      *
      *   Rev 2.5   Sep 01 2003 11:11:14   t13ae34
      *
      *   Rev 2.4   Nov 05 2002 16:30:20   t13ex57
      *AVB00636 - Erweiterung Email/URL SST
      *                                                                 
      *   Rev 2.3   Sep 05 2002 13:32:00   t13ae30                      
      *AVB00627 - ANPASSUNG SST WG. SeminarIS                           
      *                                                                 
      *   Rev 2.2   08 Jun 2000 15:35:58   T13AE27                      
      *neue Funktion DRVB0486                                           
      *                                                                 
      *   Rev 2.1   Jan 11 2000 16:22:24   T13AE30                      
      *AAS00051 - Stammdaten-DTA -> Aachen                              
      *                                                                 
      *   Rev 2.0   Feb 27 1999 15:58:20   P13SP20                      
      *NT-Umstellung                                                    
      *                                                                 
      *   Rev 1.1   17 Apr 1998 16:23:38   T13EX30                      
      *AAS00005 - Neuerstellung                                         
      *                                                                 
      *   Rev 1.0   23 Jul 1997 12:13:12   P13SP99                      
      *Initial revision.                                                
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRVB0191                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0019                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: SCHMIDT                                          * 
      *    DATUM:    03.12.1997                                       * 
      ***************************************************************** 
      *    VERSION  1                                                   
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AAS00005!03.12.97! 01 ! NEUERSTELLUNG             ! SCHMIDT  *  
      * AAS00051!26.10.99! 02 ! STAMMDATEN-DTA ---> Aachen! WILLIG   *  
      * FSL00026!05.06.00! 03 ! neu Funktion              ! P. Dürr  *  
      * AVB00627!02.09.02! 04 ! NEUE FUNKTION DRVB0596    ! WILLIG   *  
      * AVB00636!16.10.02! 05 ! NEUE FUNKTION DRVB0597    ! U. BOLL  *
      * AAS00420!27.08.03!2.5 ! NEUE FUNKTION DRVB0671    ! HEIDER   *
180211* AVB01132!18.02.11!2.7 ! NEUE FUNKTION DRVB1007    ! KLEEMEIER*
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL            
      *    PRO UPRO ÜBERGEBEN WERDEN.                                   
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN         
      *                (IN SEPARATEN COPYS PRO FUNKTION)                
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-ZWVB0019.                                                 
           05  PI-ZWVB0019-FUNKTION            PIC  X(008).             
               88 PI-ZWVB0019-DRVB0192              VALUE 'DRVB0192'.   
               88 PI-ZWVB0019-DRVB0450              VALUE 'DRVB0450'.   
050600         88 PI-ZWVB0019-DRVB0486              VALUE 'DRVB0486'.   
020902         88 PI-ZWVB0019-DRVB0596              VALUE 'DRVB0596'.   
161002         88 PI-ZWVB0019-DRVB0597              VALUE 'DRVB0597'.
270803         88 PI-ZWVB0019-DRVB0671              VALUE 'DRVB0671'.
180211         88 PI-ZWVB0019-DRVB1007              VALUE 'DRVB1007'.
      *                                                                 
      ******* ENDE COPY-MEMBER DRVB0191 *****************************
      *
