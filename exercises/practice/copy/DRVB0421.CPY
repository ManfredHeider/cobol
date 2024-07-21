      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  November 1995                             
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0421.CPY  $                                         

      *    $Revision:   2.3  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB0421.CPv  $                            
      *
      *   Rev 2.3   Apr 05 2004 11:41:16   t13ex22
      *AAS00454: Fkt. für Startseite VB-Portal
      *                                                                 
      *   Rev 2.2   Dec 10 2002 15:20:34   t13ae35                      
      *ASL00153 - neue Fkt. DRVB0428 und DRVB0623                       
      *                                                                 
      *   Rev 2.1   Dec 21 1999 09:19:02   T13AE27                      
      *neue Funktion für ZWVB0059                                       
      *                                                                 
      *   Rev 2.0   Jun 02 1999 14:44:56   P13SP20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0421                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0059  AST                   * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: Dürr                                             * 
      *    DATUM:    20.07.19969                                      * 
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * --------+--------+----+---------------------------+----------*  
      *         !20.07.99! 2.1! NEU                       ! Dürr     *  
      * --------+--------+----+---------------------------+----------*  
      * ASL00153!27.11.02! 2.2! neue Funktion DRVB0428    ! J.Hering *  
      *         !        !    ! erm. Jahres-EQF-Gruppe    !          *  
      * --------+--------+----+---------------------------+----------*  
      * AAS00425!19.08.03! 2.3!  neue Funktion DRVB0668   ! U. Boll  *  
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
       01  PI-ZWVB0059.                                                 
           05  PI-ZWVB0059-FUNKTION            PIC  X(008).             
               88 PI-ZWVB0059-DRVB0422              VALUE 'DRVB0422'.   
271102         88 PI-ZWVB0059-DRVB0428              VALUE 'DRVB0428'.   
271102         88 PI-ZWVB0059-DRVB0623              VALUE 'DRVB0623'.   
190803         88 PI-ZWVB0059-DRVB0668              VALUE 'DRVB0668'.   
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
