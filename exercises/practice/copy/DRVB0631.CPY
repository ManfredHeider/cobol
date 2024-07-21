      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0631.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/DRVB0631.CPv  $
      *
      *   Rev 2.2   Jan 24 2003 12:06:22   t13ex57
      *AAS00387 - DIM 7 Zukunft Nadeln
      *
      *   Rev 2.1   Jan 24 2003 11:45:14   t13ex57
      *AAS00387 - DIM 7 Zukunft Nadeln
      *                                                                         
      *   Rev 2.0   Dec 04 2002 12:21:32   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: SATZBESCHREIBUNG AUSGABEDATEI FUER PMAS0030   *         
      *                                                               *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: UTE BOLL                                         *         
      *****************************************************************         
      *    VERSION   01   25.02.1994                                  *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *         
      * --------------------------------------------------------------*         
      * AAS00387!20.12.02! 01 ! NEU                       ! U.BOLL    *         
      *         !        !    !                           !           *         
      *         !        !    !                           !           *         
      *                                                               *         
      * ENDE-LOGBUCH                                                  *         
      ****************************************************************          
      *                                                                         
      *   AUSGABE-DATEI                                                         
      *                                                                         
        01  DRVB0631.                                                           
           05 DRVB0631-KTY-REG              PIC  9(04).                         
           05 DRVB0631-KNR-REG              PIC  9(04).                         
           05 DRVB0631-BEZ-REG              PIC  X(10).                         
           05 DRVB0631-DIR                  PIC  9(04).                         
           05 DRVB0631-STUFE                PIC  9(04).                         
           05 DRVB0631-STUFE-BEZ            PIC  X(04).                         
           05 DRVB0631-PRAXIS               PIC  9(02).                         
           05 DRVB0631-AUS-ED-ANZ           PIC  9(02).                         
           05 DRVB0631-VBNR                 PIC  9(07).                         
           05 DRVB0631-EINH-GRP-KORR        PIC -9(09),9.                       
           05 DRVB0631-EINH-EIGEN           PIC -9(09),9.                       
           05 DRVB0631-EINH-GES-KORR        PIC -9(09),9.                       
           05 DRVB0631-EINH-GES-MAX         PIC -9(09),9.                       
           05 DRVB0631-BED-ERF              PIC  X.                             
           05 DRVB0631-NAME                 PIC  X(40).                         
           05 DRVB0631-VORNAME              PIC  X(20).                         
           05 DRVB0631-TAGESDATUM-ISO       PIC  X(10).                         
      *                                                                         
      ******** ENDE COPY-MEMBER (DRVB0631) ************                         
      *                                                                         
