      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2003                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0654.CPY  $                                         


      *    $Revision:   2.4  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB0654.CPv  $                            
      *
      *   Rev 2.4   Nov 04 2005 11:57:16   T13AE27
      *AAS00572: Siegeradler 2005
      *
      *   Rev 2.3   May 23 2005 14:21:38   t13ae27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *                                                                 
      *   Rev 2.2   Jan 28 2004 10:59:12   t13ex22                      
      *AAS00443: WBW2004 erm. ND über gesamten Basiszeitraum            
      *                                                                 
      *   Rev 2.1   Jun 10 2003 14:20:04   t13ex22                      
      *AVB00666: Einheitenberechnung Basis-/Steigerungszeitraum         
      *                                                                 
      *   Rev 2.0   Feb 17 2003 10:57:32   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0654                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS FUNKTIONS-OZP UPVB0130               * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: MUND                                             * 
      *    DATUM:    22.04.2003                                       * 
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AVB00666!22.04.03! 01 ! NEU                       ! MUND     *  
      * AAS00443!28.01.04! 2.1! DRVB0698 NEU              ! MUND     *  
      * AAS00531!13.04.05! 2.2! DRVB0772 NEU              ! MUND     *
      * AAS00572!12.10.05! 2.3! DRVB0799 NEU              ! Duerr    *
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
       01  PI-UPVB0130.                                                 
           05  PI-UPVB0130-FUNKTION            PIC  X(008).             
               88 PI-UPVB0130-DRVB0655              VALUE 'DRVB0655'.   
               88 PI-UPVB0130-DRVB0658              VALUE 'DRVB0658'.   
280104         88 PI-UPVB0130-DRVB0698              VALUE 'DRVB0698'.   
130405         88 PI-UPVB0130-DRVB0772              VALUE 'DRVB0772'.
121005         88 PI-UPVB0130-DRVB0799              VALUE 'DRVB0799'.
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
