      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0623.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/DRVB0623.CPv  $
      *
      *   Rev 2.2   Dec 10 2002 16:16:38   t13ae35
      *ASL00153 - Zeichen in Spalte 84 entfernt
      *
      *   Rev 2.1   Dec 10 2002 15:26:06   t13ae35
      *DRVB0623 - neu, erm. letzten JEQF zum Stichtag
      *                                                                         
      *   Rev 2.0   Dec 04 2002 12:20:56   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *---------------------------------------------------------------*         
      *    COPY-MEMBER: DRVB0623                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0059                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *    Zu einer gegebenen VB-Nr und einem gegebenen Stichtag      *         
      *    wird der letzte JEQF (12-Monate-EQF der Gruppe) ermittelt  *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: J.Hering                                         *         
      *    DATUM:    27.11.2002                                       *         
      *****************************************************************         
      *    VERSION  1                                                           
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00153!27.11.02! 2.1! Neu, zu gegebenen VB und  ! Hering   *          
      *         !        !    ! Stichtag den JEQF ermi.   !          *          
      * --------+--------+----+---------------------------+----------*          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    FÜR DIE FUNKTION DRVB0623                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      ****************************************************************          
      *                                                                         
       01 PI-DRVB0623.                                                          
          05 PI-DRVB0623-MA-ST-NR           PIC S9(009) BINARY.                 
          05 PI-DRVB0623-ZEITPUNKT-ISO      PIC  X(010).                        
      *                                                                         
       01 PO-DRVB0623.                                                          
                                                                                
          05 PO-DRVB0623-RC                 PIC  X(003).                        
             88 PO-DRVB0623-RC-OK                VALUE 'OK '.                   
             88 PO-DRVB0623-RC-LEER              VALUE 'LER'.                   
             88 PO-DRVB0623-RC-EOF               VALUE 'EOF'.                   
             88 PO-DRVB0623-RC-NOT-FOUND         VALUE 'NFD'.                   
                                                                                
          05 PO-DRVB0623-DATEN.                                                 
             10 PO-DRVB0623-AST-JEQF-GR     PIC  S9(3)V99 COMP-3.               
      *                                                                         
      ***** ENDE COPY-MEMBER DRVB0623 *****************************             
