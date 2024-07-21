      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0125.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0125.CPv  $
      *
      *   Rev 2.2   Feb 28 2012 12:10:24   t13ex57
      *AVB01206: Umbenennung BD (UBO)
      *
      *   Rev 2.1   Mar 05 2003 10:08:38   t13ae13
      *AVB00648: Dim 7ukunft - Neue Stufen 
      *
      *   Rev 2.0   Feb 27 1999 11:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Apr 11 1997 08:13:04   T13AE31
      *KORREKTUR VBA
      *
      *   Rev 1.1   Apr 09 1997 16:46:28   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:19:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: KONSTANTEN KLARTEXT FÜR STUFEN (DRAT0125)     * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: GIESE                                            * 
      *    DATUM:    23.01.1997                                       * 
      ***************************************************************** 
      *    VERSION   1    20.04.95 11:40:25
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !23.04.93! 01 ! NEU                       ! GIESE    *  
      * AFPA0008!10.04.97!    ! VBA1 AUFGENOMMEN          ! GIESE    *  
      * AFPA0008!10.04.97!    ! VBA4 KEINE GüLTIGE STUFE  ! AUERBACH *  
      * AVB00648!14.02.03!2.1 ! VBAS und ALP  aufgenommen ! Walb     *  
140212* AVB01026!14.02.12!2.2 ! Umbenennung BD => PD      ! U.BOLL   *
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *   AUSPRÄGUNGEN STUFE                                            
      *                                                                 
       01  K-STF-VM01-X             PIC X(04)  VALUE 'VM01'.            
       01  K-STF-VM02-X             PIC X(04)  VALUE 'VM02'.            
       01  K-STF-VM03-X             PIC X(04)  VALUE 'VM03'.            
       01  K-STF-VM04-X             PIC X(04)  VALUE 'VM04'.            
       01  K-STF-VM09-X             PIC X(04)  VALUE 'VM09'.            
140203 01  K-STF-VBAS-X             PIC X(04)  VALUE 'VBAS'.            
       01  K-STF-VBA-X              PIC X(04)  VALUE 'VBA '.            
100497 01  K-STF-VBA1-X             PIC X(04)  VALUE 'VBA1'.            
       01  K-STF-VBA2-X             PIC X(04)  VALUE 'VBA2'.            
       01  K-STF-VBA3-X             PIC X(04)  VALUE 'VBA3'.            
104097*01  K-STF-VBA4-X             PIC X(04)  VALUE 'VBA4'.            
140203 01  K-STF-ALP-X              PIC X(04)  VALUE 'ALP '.            
       01  K-STF-AL-X               PIC X(04)  VALUE 'AL  '.            
       01  K-STF-GA-X               PIC X(04)  VALUE 'GA  '.            
       01  K-STF-RGS-X              PIC X(04)  VALUE 'RGS '.            
       01  K-STF-GS-X               PIC X(04)  VALUE 'GS  '.            
       01  K-STF-HGS-X              PIC X(04)  VALUE 'HGS '.            
       01  K-STF-RD1-X              PIC X(04)  VALUE 'RD1 '.            
       01  K-STF-RD2-X              PIC X(04)  VALUE 'RD2 '.            
140212*01  K-STF-BD-X               PIC X(04)  VALUE 'BD  '.
140212 01  K-STF-BD-X               PIC X(04)  VALUE 'PD  '.            
       01  K-STF-D-X                PIC X(04)  VALUE 'D   '.            
       01  K-STF-LD-X               PIC X(04)  VALUE 'LD  '.            
      *                                                                 
      ******** ENDE COPY-MEMBER KONSTANTEN STUFE (DRAT0125)********     
