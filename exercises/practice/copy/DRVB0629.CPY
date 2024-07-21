      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0629.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/DRVB0629.CPv  $
      *
      *   Rev 2.2   Jan 24 2003 12:05:56   t13ex57
      *AAS00387 - DIM 7 Zukunft Nadeln
      *
      *   Rev 2.1   Jan 24 2003 11:44:44   t13ex57
      *AAS00387 - DIM 7 Zukunft Nadeln 
      *                                                                         
      *   Rev 2.0   Dec 04 2002 12:21:22   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      ***************************************************************           
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      * AAS00387!20.12.02! 01 ! NEU                       !U. BOLL   *          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                              *          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    FÜR DIE FUNKTION DRVB0629                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      *                                                              *          
      ****************************************************************          
       01 PI-DRVB0629.                                                          
          05 PI-DRVB0629-DATEN.                                                 
             10 PI-DRVB0629-MA-ST-NR         PIC S9(009) COMP.                  
                                                                                
      *                                                                         
       01 PO-DRVB0629.                                                          
          05 PO-DRVB0629-RC                  PIC  X(003).                       
             88 PO-DRVB0629-RC-OK            VALUE 'OK '.                       
             88 PO-DRVB0629-RC-NFD           VALUE 'NFD'.                       
          05 PO-DRVB0629-DATEN.                                                 
             10 PO-DRVB0629-ED-ANZ           PIC S9(4) COMP.                    
      ******** Ende Copy-Member **************************************          
