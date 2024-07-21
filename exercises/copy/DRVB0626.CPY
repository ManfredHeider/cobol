      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0626.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/DRVB0626.CPv  $
      *
      *   Rev 2.2   Jan 02 2003 17:24:56   t13ae35
      *ASL00153 - CR/LF in Spalte 83 entfernt
      *
      *   Rev 2.1   Jan 02 2003 15:28:12   t13ae35
      *ASL00153 - neu
      *                                                                         
      *   Rev 2.0   Dec 04 2002 12:21:08   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRVB0627                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0088                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: J.Hering                                         *         
      *    DATUM:    11.12.2002                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00153!11.12.02! 2.2! neu, Funktion DRVB0627    ! J.Hering *          
      *         !        !    ! gesamtes Tupel aus BOZA   !          *          
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
       01  PI-ZWVB0088.                                                         
           05  PI-ZWVB0088-FUNKTION            PIC  X(008).                     
               88 PI-ZWVB0088-DRVB0627              VALUE 'DRVB0627'.           
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
