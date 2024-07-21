      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DWAT0007.CPv  $
      *
      *   Rev 2.1   Jan 24 2005 15:10:22   t13ex22
      *FVB00598: VB22 - Anpassung TS-Queue wg. MA-Erweiterung
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Jun 24 1996 13:00:42   T13AE26
      *AFPA0001: Anpassung an MF-Cobol-WB (VALUE ZERO)
      *
      *   Rev 1.0   Nov 04 1995 09:30:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DWAT0007      TSQUEUE-SERVICE                 * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 
      *    DATUM:    18.03.1993                                       * 
      ***************************************************************** 
      *    VERSION   02   16.09.93                                   *  
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !24.04.93! 01 ! NEU                       ! SCHOELLH.*  
      *         !09.09.93! 02 ! PI-...TSQ-DATA UND        ! SCHOELLH.*  
      *         !        !    ! PO-...TSQ-DATA + 100 BYTES!          *  
      *         !12.02.96!    ! VALUE ZERO F. WORKBENCH   ! GERLACH  *  
      *FVB00598 !20.01.05!2.1 ! TS-QUEUE VERLÄNGERT       ! MUND     *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    SCHNITTSTELLE FUER TS-QUEUE-SERVICE:                         
      *    LOESCHEN VON / SCHREIBEN IN / LESEN AUS TS-QUEUE             
      *                                                                 
      *    ABHÄNGIG VON ZI-DWAT0007-TSQ-SERVICE                         
      *    MUESSEN BEIM AUFRUF ÜBERGEBEN WERDEN:                        
      *        -----------------------+-------+-------+-------+-------+ 
      *        TSQ-SERVICE            !DELETE ! WRITE !REWRITE! READ  ! 
      *        -----------------------+-------+-------+-------+-------+ 
      *        ZI-DWAT0007-TSQ-SERVICE!   X   !   X   !   X   !   X   ! 
      *        ZI-DWAT0007-TSQ-PLACE  !       !  (A)  !  (A)  !       ! 
      *        ZI-DWAT0007-TSQ-NAME   !   X   !   X   !   X   !   X   ! 
      *        ZI-DWAT0007-TSQ-ITEM   !       !       !   X   !  (B)  ! 
      *        ZI-DWAT0007-TSQ-DATA   !       !   X   !   X   !       ! 
      *        -----------------------+-------+-------+-------+-------+ 
      *    WERDEN VOM SERVICE ZURÜCKGELIEFERT:                          
      *        -----------------------+-------+-------+-------+-------+ 
      *        ZO-DWAT0007-TSQ-RC     !   X   !   X   !   X   !   X   ! 
      *        ZO-DWAT0007-TSQ-DATA   !       !       !       !   X   ! 
      *        -----------------------+-------+-------+-------+-------+ 
      *                                                                 
      *    ANMERKUNGEN:                                                 
      *        (A) DEFAULT BEI TSQ-PLACE = MAIN                         
      *        (B) DEFAULT BEI TSQ-ITEM  = NEXT (NUR BEI READ)          
      *                                                                 
      *    MÖGLICHE RETURN-WERTE:                                       
      *        -----------------------+-------+-------+-------+-------+ 
      *        TSQ-SERVICE            !DELETE ! WRITE !REWRITE! READ  ! 
      *        -----------------------+-------+-------+-------+-------+ 
      *        TSQ-RC-OK              !   X   !   X   !   X   !   X   ! 
      *        TSQ-RC-ITEMERR         !       !       !   X   !   X   ! 
      *        -----------------------+-------+-------+-------+-------+ 
      *                                                                 
      * -------------------------------------------------------------   
       01  ZI-DWAT0007-INPUT.                                           
120296*    05  ZI-DWAT0007-TSQ-SERVICE        PIC  9(001).              
120296     05  ZI-DWAT0007-TSQ-SERVICE        PIC  9(001) VALUE 0.      
               88  TSQ-SERVICE-DELETE                      VALUE 1.     
               88  TSQ-SERVICE-WRITE                       VALUE 2.     
               88  TSQ-SERVICE-REWRITE                     VALUE 3.     
               88  TSQ-SERVICE-READ                        VALUE 4.     
120296*    05  ZI-DWAT0007-TSQ-PLACE          PIC  9(001).              
120296     05  ZI-DWAT0007-TSQ-PLACE          PIC  9(001)  VALUE 0.
               88  TSQ-PLACE-MAIN                          VALUE 0.     
      *              ZUR ZEIT NICHT REALISIERT:                         
      *        * 88  TSQ-PLACE-AUX                           VALUE 1.   
                                                                        
           05  ZI-DWAT0007-TSQ-NAME           PIC  X(008).              
           05  ZI-DWAT0007-TSQ-ITEM           PIC S9(004) BINARY.       
200105*    05  ZI-DWAT0007-TSQ-DATA           PIC  X(300).              
200105     05  ZI-DWAT0007-TSQ-DATA           PIC  X(340).              
                                                                        
       01  ZO-DWAT0007-OUTPUT.                                          
120296*    05  ZO-DWAT0007-TSQ-RC             PIC  9(001).              
120296     05  ZO-DWAT0007-TSQ-RC             PIC  9(001)  VALUE 0.     
               88  TSQ-RC-OK                               VALUE 0.     
               88  TSQ-RC-ITEMERR                          VALUE 1.     
200105*    05  ZO-DWAT0007-TSQ-DATA           PIC  X(300).              
200105     05  ZO-DWAT0007-TSQ-DATA           PIC  X(340).              
      *                                                                 
      ******* ENDE COPY-MEMBER DWAT0007 (TSQUEUE-SERVICE)     ********* 
CCC009*    EJECT                                                        
