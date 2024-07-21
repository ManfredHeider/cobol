      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  November 1995                             
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRAT0090.CPY  $                                         




      *    $Revision:   2.7  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRAT0090.CPv  $                            
      *
      *   Rev 2.7   Nov 29 2010 15:36:30   t13ex13
      *ASL00367: SL-Dialog für Leistungszuschuss Allfinanz
      *
      *   Rev 2.6   Jun 22 2005 09:41:10   t13ae27
      *ASL00204: K-Leistungszuschuss
      *
      *   Rev 2.5   Jul 09 2004 11:30:46   t13ex57
      *ASL00184: Bestandsleistungszuschuss OLZZ
      *
      *   Rev 2.4   May 19 2004 11:08:40   t13ex57
      *MAP - Manageraufbauplan
      *
      *   Rev 2.3   Jan 02 2003 15:19:48   t13ae35
      *ASL00153 - neue OLZ-Typen für BOZ, OLBN und ZBON
      *                                                                 
      *   Rev 2.2   Apr 17 2000 16:20:34   T13AE30                      
      *ASL00049 - DIVERSE AUSWERTUNGEN DIM7                             
      *                                                                 
      *   Rev 2.1   Mar 29 2000 14:40:58   T13AE30                      
      *ASL00049 - Dimension 7 Aufbau-OLZ                                
      *                                                                 
      *   Rev 2.0   Feb 27 1999 11:57:18   P13SP20                      
      *NT-Umstellung                                                    
      *                                                                 
      *   Rev 1.0   Nov 04 1995 08:51:18   P13SP99                      
      *Initial revision.                                                
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRAT0090   KNR-KONST. KTY-56   OLZ-TYP        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: SCHÖLLHAMMER                                     * 
      *    DATUM:    02.02.1995                                       * 
      ***************************************************************** 
      *    VERSION   1    23.02.95 15:03:48                             
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * --------+--------+----+---------------------------+----------*  
      * AFVB0183!02.02.95! 01 ! NEU                       ! SCHÖLLH. *  
      * --------+--------+----+---------------------------+----------*  
      * ASL00050!22.03.00! 02 ! ANPASSUNG NEUE OLZ-TYPEN  ! WILLIG   *  
      *         !        !    ! 6(AUFBAU-OLZ) U. 7 (OLB)  !          *  
      * --------+--------+----+---------------------------+----------*  
      * ASL00049!17.04.00! 03 ! ANPASSUNG NEUEN OLZ-TYP   ! WILLIG   *  
      *         !        !    ! K-KNR-ORG-RGS = 8         !          *  
      * --------+--------+----+---------------------------+----------*  
      * ASL00153!10.12.02! 2.3! neuer OLZ-Typen für BOZ,  !          *  
      *         !        !    ! OLB-Neu und Zusatzbonus   ! J.Hering *  
      * --------+--------+----+---------------------------+----------*
      * ASL00183!10.05.04! 2.4! neuer OLZ-Typ für MAP     ! U. Boll  *
      * --------+--------+----+---------------------------+----------*
      * ASL00184!24.06.04! 2.5! neuer OLZ-Typ für LZB     ! U. Boll  *
      * --------+--------+----+---------------------------+----------*
      * ASL00204!31.05.05! 2.6! neuer KNR-Typ für KLZ     ! P. Duerr *
      * --------+--------+----+---------------------------+----------*
      * ASL00367!17.11.10! 2.7! NEUER KNR-TYP FÜR LZU ALLF!GUTFLEISCH*
      * --------+--------+----+---------------------------+----------*
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *   SCHLÜSSELWERTE KTY-56   OLZ-TYP                               
      *                                                                 
       01  FILLER.                                                      
           05  K-KNR-BUERO                    PIC S9(004) COMP VALUE  1.
           05  K-KNR-LEISTUNG                 PIC S9(004) COMP VALUE  2.
           05  K-KNR-ORG                      PIC S9(004) COMP VALUE  3.
           05  K-KNR-PARTNER-BONUS-ZAHLUNG    PIC S9(004) COMP VALUE  4.
           05  K-KNR-PARTNER-BONUS-ERMITTELT  PIC S9(004) COMP VALUE  5.
220300     05  K-KNR-AUFBAU-OLZ               PIC S9(004) COMP VALUE  6.
220300     05  K-KNR-OLB                      PIC S9(004) COMP VALUE  7.
170400     05  K-KNR-ORG-RGS                  PIC S9(004) COMP VALUE  8.
101202     05  K-KNR-BOZ                      PIC S9(004) COMP VALUE  9.
101202     05  K-KNR-OLB-NEU                  PIC S9(004) COMP VALUE 10.
101202     05  K-KNR-OLB-ZUSATZ-BONUS         PIC S9(004) COMP VALUE 11.
100504     05  K-KNR-MAP                      PIC S9(004) COMP VALUE 12.
240604     05  K-KNR-LZB                      PIC S9(004) COMP VALUE 13.
310505     05  K-KNR-KLZ                      PIC S9(004) COMP VALUE 14.
171110     05  K-KNR-LZA                      PIC S9(004) COMP VALUE 15.
      *
      ***** ENDE COPY-MEMBER DRAT0090  KNR-KONST. ART PROV.-TYP   .*****
