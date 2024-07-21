      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 1995                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                            
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0428.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/MF/CPY/DRVB0428.CPv  $
      *
      *   Rev 2.2   Dec 10 2002 16:15:22   t13ae35
      *ASL00153 - Zeichen an Spalte 84 entfernt
      *
      *   Rev 2.1   Dec 10 2002 15:22:28   t13ae35
      *ASL00153 - neu, erm. JEQF zu VB und Stichtag 
      *                                                                         
      *   Rev 2.0   Jun 02 1999 14:44:58   P13SP20                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *    COPY-MEMBER: DRVB0428                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0059                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *    Zu einer gegebenen VB-Nr und einem gegebenen Stichtag      *         
      *    wird der JEQF (12-Monate-EQF der Gruppe) ermittelt         *         
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
      *    FÜR DIE FUNKTION DRVB0428                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      ****************************************************************          
      *                                                                         
       01 PI-DRVB0428.                                                          
          05 PI-DRVB0428-MA-ST-NR           PIC S9(009) BINARY.                 
          05 PI-DRVB0428-ZEITPUNKT-ISO      PIC  X(010).                        
      *                                                                         
       01 PO-DRVB0428.                                                          
                                                                                
          05 PO-DRVB0428-RC                 PIC  X(003).                        
             88 PO-DRVB0428-RC-OK                VALUE 'OK '.                   
             88 PO-DRVB0428-RC-LEER              VALUE 'LER'.                   
             88 PO-DRVB0428-RC-EOF               VALUE 'EOF'.                   
             88 PO-DRVB0428-RC-NOT-FOUND         VALUE 'NFD'.                   
                                                                                
          05 PO-DRVB0428-DATEN.                                                 
             10 PO-DRVB0428-AST-JEQF-GR     PIC  S9(3)V99 COMP-3.               
      *                                                                         
      ***** ENDE COPY-MEMBER DRVB0428 *****************************             
