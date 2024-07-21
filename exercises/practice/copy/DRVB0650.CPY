      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2003                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   drvb0650.cpy  $                                 
      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/cpy/DRVB0650.CPv  $                                
      *
      *   Rev 2.1   Apr 22 2003 16:22:28   t13ae35
      *AVB00618 - neu, Funktionencopy zu ZXVB0087
      *                                                                 
      *   Rev 2.0   Feb 17 2003 10:57:08   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0650                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0087                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: J.Hering                                         * 
      *    DATUM:    04.04.2003                                       * 
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * --------+--------+----+---------------------------+----------*  
      * AVB00618!04.04.03! 2.1! neu, Funktion DRVB0651    ! J.Hering *  
      *         !        !    ! Jahreseinkommen je VB     !          *  
      * --------+--------+----+---------------------------+----------*  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
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
       01  PI-ZXVB0087.                                                 
           05  PI-ZXVB0087-FUNKTION            PIC  X(008).             
               88 PI-ZXVB0087-DRVB0651              VALUE 'DRVB0651'.   
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
