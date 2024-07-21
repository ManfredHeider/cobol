      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0624.CPY  $                                         
      *    $Revision:   2.3  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRVB0624.CPv  $
      *
      *   Rev 2.3   Apr 23 2003 16:13:16   t13ex22
      *ASL00156: Neuer Dialog SL01
      *
      *   Rev 2.2   Jan 02 2003 17:24:36   t13ae35
      *ASL00153 - CR/LF in Spalte 83 entfernt
      *
      *   Rev 2.1   Jan 02 2003 15:26:22   t13ae35
      *ASL00153 - neu
      *                                                                         
      *   Rev 2.0   Dec 04 2002 12:21:00   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRVB0624                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0088                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: J.Hering                                         *         
      *    DATUM:    11.12.2002                                       *         
      *****************************************************************         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00153!11.12.02! 2.2! neu, Funktion DRVB0625    ! J.Hering *          
      *         !        !    ! BOZ-Ansprüche je Monat    !          *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00156!09.04.03! 2.3! NEUE FUNKTION             ! MUND     *
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
       01  PI-ZXVB0088.                                                         
           05  PI-ZXVB0088-FUNKTION            PIC  X(008).                     
               88 PI-ZXVB0088-DRVB0625              VALUE 'DRVB0625'.
090403         88 PI-ZXVB0088-DRVB0653              VALUE 'DRVB0653'.
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
