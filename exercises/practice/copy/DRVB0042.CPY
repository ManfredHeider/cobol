      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0042.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0042.CPv  $
      *
      *   Rev 2.1   Jan 24 2005 15:10:30   t13ex22
      *FVB00598: VB22 - Anpassung TS-Queue wg. MA-Erweiterung
      *
      *   Rev 2.0   Feb 27 1999 15:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:11:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  
      *    COPY-MEMBER: DRVB0042  SNST UPVB0041 BEARB TSQ-ZWISPEICHER*  
      *    -------------------------------------------------------   *  
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                 *  
      *    DATUM:    10.08.1993                                      *  
      ****************************************************************  
      *    VERSION   2    26.08.94 16:33:54
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !10.08.93! 01 ! NEU                       ! SCHOELLH.*  
      *         !06.09.93!    ! RC '13' GESTRICHEN        ! SCHOELLH.*  
      *         !10.09.93!    ! ..-SATZ-IN UND ..-SATZ-OUT! SCHOELLH.*  
      *         !        !    ! JE 100 BYTE GRÖSSER       !          *  
      * AFRZ0013!26.08.94! 02 ! KUS-SYSTEM / RACF         ! WIECHERS *  
      * FVB00598!20.01.05! 2.1! TSQ VERLÄNGERT            ! MUND     *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    AUFRUF-PARAMETER FUER UPRO DRVB0041:                         
      *    BEARBEITEN TSQ-ZWISCHENSPEICHER                              
      *    - PARKEN     ZWISCHENSPEICHERN DB2-INHALTE IN TSQ (SATZWEISE)
      *    - SCHREIBEN  SCHREIBEN TSQ-INHALT NACH DB2 (ALLE SÄTZE)      
      *                 (KEIN ANSCHLIESSENDES LOESCHEN DER TSQ)         
      *    - HOLEN      HOLEN TSQ-INHALT (SATZWEISE) FUER AUFRUF. PROG. 
      *    - LOESCHEN   LOESCHEN TS-QUEUE                               
      *                                                                 
      *    ABHAENGIG VON PI-UPVB0042-FKT SIND ZU FUELLEN:               
      *                                                                 
      *    FKT                         ! PO! PW! PC! S ! HO! HN! L !    
      *    ----------------------------+---+---+---+---+---+---+---+    
      *    PI-UPVB0041-FKT             ! X ! X ! X ! X ! X ! X ! X !    
      *    PI-UPVB0041-TSQ-NAME        ! X ! X ! X ! X ! X ! X ! X !    
      *    PI-UPVB0041-ANZAHL-IN       !   !   !   ! X ! X !   !   !    
      *    PI-UPVB0041-SATZ-IN         !   ! X !   !   !   !   !   !    
      *    PI-UPVB0041-PERSNR          !   !   !   ! X !   !   !   !    
      *    PI-UPVB0041-TRMID           !   !   !   ! X !   !   !   !    
      *    PI-UPVB0041-CICS-DATUM-ISO  !   !   !   ! X !   !   !   !    
      *    ----------------------------+---+---+---+---+---+---+---+    
      *                                                                 
      *    ABHAENGIG VON PI-UPVB0042-FKT WERDEN ZURÜCKGELIEFERT:        
      *                                                                 
      *    - ERR-RC (ETC)                                               
      *    - BEI ERR-RC-OK:                                             
      *    FKT                         ! PO! PW! PC! S ! HO! HN! L !    
      *    ----------------------------+---+---+---+---+---+---+---+    
      *    PO-UPVB0041-RC              ! X ! X ! X ! X ! X ! X ! X !    
      *       UPVB41-RC-OK             !  +!  +!  +!  +!  +!  +!  +!    
      *       UPVB41-RC-EOF            !   !   !   !   !   !  +!   !    
      *       UPVB41-RC-FKT-UNGUELT    !  +!  +!  +!  +!  +!  +!  +!    
      *       UPVB41-RC-TSQNAME-FEHLT  !  +!  +!  +!  +!  +!  +!  +!    
      *       UPVB41-RC-ANZAHL-FEHLT   !   !   !   !  +!  +!   !   !    
      *       UPVB41-RC-SATZIN-FEHLT   !   !  +!   !   !   !   !   !    
      *       UPVB41-RC-PERSNR-FEHLT   !   !   !   !  +!   !   !   !    
      *       UPVB41-RC-TRMID-FEHLT    !   !   !   !  +!   !   !   !    
      *       UPVB41-RC-DATUM-FEHLT    !   !   !   !  +!   !   !   !    
      *       UPVB41-RC-KEIN-OPEN      !   !  +!  +!   !   !  +!   !    
      *       UPVB41-RC-CLOSE-OHNE-WR  !   !   !  +!   !   !   !   !    
      *       UPVB41-RC-ANZAHL-UNGUELT !   !   !   !  +!  +!   !   !    
      *       UPVB41-RC-DB2-USER-AEND  !   !   !   !  +!   !   !   !    
      *       UPVB41-RC-DB2-DUP-EINTR  !   !   !   !  +!   !   !   !    
      *    PO-UPVB0041-ANZAHL-OUT      !   !   ! X !   !   !   !   !    
      *    PO-UPVB0041-SATZ-OUT        !   !   !   !   !   ! X !   !    
      *    ----------------------------+---+---+---+---+---+---+---+    
      *                                  X = WIRD ZURÜCKGELIEFERT       
      *                                  + = MÖGLICHE WERTE             
      *                                                                 
       01  PI-UPVB0041-EINGABE.                                         
           05  PI-UPVB0041-FKT                 PIC  X(002).             
               88  UPVB41-FKT-PARKEN-OPEN                  VALUE 'PO'.  
               88  UPVB41-FKT-PARKEN-WRITE                 VALUE 'PW'.  
               88  UPVB41-FKT-PARKEN-CLOSE                 VALUE 'PC'.  
               88  UPVB41-FKT-SCHREIBEN                    VALUE 'S '.  
               88  UPVB41-FKT-HOLEN-OPEN                   VALUE 'HO'.  
               88  UPVB41-FKT-HOLEN-NEXT                   VALUE 'HN'.  
               88  UPVB41-FKT-LOESCHEN                     VALUE 'L '.  
           05  PI-UPVB0041-TSQ-NAME            PIC  X(008).             
           05  PI-UPVB0041-ANZAHL-IN           PIC  9(004).             
200105*    05  PI-UPVB0041-SATZ-IN             PIC  X(300).             
200105     05  PI-UPVB0041-SATZ-IN             PIC  X(340).             
260894*    05  PI-UPVB0041-PERSNR              PIC  9(007) COMP-3.      
260894     05  PI-UPVB0041-USERID              PIC  X(007).             
           05  PI-UPVB0041-TRMID               PIC  X(004).             
           05  PI-UPVB0041-CICS-DATUM-ISO      PIC  X(010).             
                                                                        
       01  PO-UPVB0041-AUSGABE.                                         
           05  PO-UPVB0041-RC                  PIC  9(002).             
               88  UPVB41-RC-OK                            VALUE 01.    
               88  UPVB41-RC-EOF                           VALUE 02.    
               88  UPVB41-RC-FKT-UNGUELT                   VALUE 11.    
               88  UPVB41-RC-TSQNAME-FEHLT                 VALUE 12.    
      *      * 88  UPVB41-RC-ANZAHL-FEHLT                  VALUE 13.    
               88  UPVB41-RC-SATZIN-FEHLT                  VALUE 14.    
260894*        88  UPVB41-RC-PERSNR-FEHLT                  VALUE 15.    
260894         88  UPVB41-RC-USERID-FEHLT                  VALUE 15.    
               88  UPVB41-RC-TRMID-FEHLT                   VALUE 16.    
               88  UPVB41-RC-DATUM-FEHLT                   VALUE 17.    
               88  UPVB41-RC-KEIN-OPEN                     VALUE 21.    
               88  UPVB41-RC-CLOSE-OHNE-WR                 VALUE 22.    
               88  UPVB41-RC-ANZAHL-UNGUELT                VALUE 23.    
               88  UPVB41-RC-DB2-USER-AEND                 VALUE 31.    
               88  UPVB41-RC-DB2-DUP-EINTR                 VALUE 32.    
               88  UPVB41-RC-FKT-N-UNTERST                 VALUE 99.    
           05  PO-UPVB0041-ANZAHL-OUT          PIC  9(004).             
200105*    05  PO-UPVB0041-SATZ-OUT            PIC  X(300).             
200105     05  PO-UPVB0041-SATZ-OUT            PIC  X(340).             
      *                                                                 
      ******** ENDE COPY-MEMBER DRVB0042 (SNST UPVB0041)   ************ 
