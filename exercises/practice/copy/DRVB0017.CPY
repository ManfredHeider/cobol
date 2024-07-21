      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0017.CPv  $
      *
      *   Rev 2.1   May 03 2010 08:03:20   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung 
      *
      *   Rev 2.0   Feb 27 1999 15:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:06:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0017                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE UPVB0026 ERM ÜBERSTELLUNGSVERHÄLTNIS         * 
      *                               + ED-DATEN                      * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: GIESE                                            * 
      *    DATUM:    24.04.1993                                       * 
      ***************************************************************** 
      *    VERSION   02   16.09.93                                   *  
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !24.04.93! 01 ! NEU                       ! GIESE    *  
      *         !16.09.93! 02 ! ERWEITERUNG DES PO-BEREICH! GIESE    *  
      * AVB01031!23.02.10!2.1 ! neu: PI-UP...-BVH-UNTST-NR! Walb     *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *  VERARBEITUNG DES UPVB0026  (VORBESETZUNG DES FUNKTIONSFELDES)  
      *  -------------------------                                      
      *  1. FKT-OPEN                                                    
      *  2. FKT-READ      (WENN EINSTIEGS-ST-NR = MA)                   
      *  2. FKT-READ-ED   (WENN EINSTIEGS-ST-NR = ED)                   
      *  3. FKT-READ-NEXT (MEHRFACH IN EINER SCHLEIFE)                  
      *  4. FKT-CLOSE                                                   
      *                                                                 
      *                MÖGLICHE RETCODES                                
      *                ! OK ! NOK ! EOF!                                
      *  FKT-OPEN      ! J  !  J  !  - !                                
      *  FKT-READ      ! J  !  J  !  J !                                
      *  FKT-READ-ED   ! J  !  J  !  J !                                
      *  FKT-READ-NEXT ! J  !  J  !  J !                                
      *  FKT-CLOSE     ! J  !  J  !  - !                                
      *                                                                 
      *   PI-UPVB0026-ST-NR          DIE ST-NR, FÜR DIE DIE             
      *                              ÜBERSTELLUNG GESUCHT WIRD          
      *                              KANN EINE ED-ST-NR SEIN            
      *   PI-UPVB0026-FUNKTION       WENN ST-NR (=MA) DANN FUNKT: READ  
      *                              WENN ST-NR (=ED)   FUNKT.: READ-ED 
      *                                                                 
      *   PO-UPVB0026-UEBER-ST-NR    DIE ST-NR DES ÜBERSTELLTEN,        
      *                              KANN EINE ED-ST-NR SEIN            
      *                                                                 
      *   PO-UPVB0026-MS-BEZ                                            
      *                                                                 
      *   PO-UPVB0026-MA-ST-NR       DIE MA-ST-NR DER ÜBERSTELLUNG      
      *                                                                 
      *   PO-UPVB0026-TAB-NAME       BVH, EDD, EDED (= NAME DER TABELLE,
      *                              AUS DER DER ÜBERSTELLTE ERMITTELT  
      *                              WURDE                              
      *                                                                 
       01  PI-UPVB0026.                                                 
           05 PI-UPVB0026-ST-NR           PIC S9(09) COMP.              
           05 PI-UPVB0026-FUNKTION        PIC  X(02).                   
              88 PI-UPVB0026-FKT-OPEN       VALUE 'OP'.                 
              88 PI-UPVB0026-FKT-READ       VALUE 'RD'.                 
              88 PI-UPVB0026-FKT-READ-ED    VALUE 'RE'.                 
              88 PI-UPVB0026-FKT-READ-NEXT  VALUE 'RN'.                 
              88 PI-UPVB0026-FKT-CLOSE      VALUE 'CL'.                 
           05 PI-UPVB0026-UEBERSTELLT-ZTP PIC  X(10).                   
230210     05 PI-UPVB0026-BVH-UNTST-NR    PIC S9(09) COMP.              
      *                                                                 
       01  PO-UPVB0026.                                                 
           05 PO-UPVB0026-RETCODE         PIC X(03).                    
           05 PO-UPVB0026-LFD-NR          PIC 9(02).                    
           05 PO-UPVB0026-UEBER-ST-NR     PIC 9(07).                    
           05 PO-UPVB0026-MS-BEZ          PIC X(04).                    
           05 PO-UPVB0026-ST-NAME         PIC X(40).                    
           05 PO-UPVB0026-TUPEL-ED        PIC X(99).                    
           05 PO-UPVB0026-MA-ST-NR        PIC 9(07).                    
           05 PO-UPVB0026-UEBER-ST-TYP    PIC S9(04) COMP.              
           05 PO-UPVB0026-TAB-NAME        PIC  X(04).                   
      *                                                                 
      ******** ENDE COPY-MEMBER (DRVB0017) *******************          
