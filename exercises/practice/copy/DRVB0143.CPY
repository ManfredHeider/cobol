      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0143.CPY  $
 
      *    $Revision:   2.23  $
      *
      *    $Log:   L:/mf/CPY/DRVB0143.CPv  $
      *
      *   Rev 2.23   Dec 21 2020 17:33:48   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.22   Sep 09 2016 11:36:26   t13ae13
      *AVB01400: Projekt Lucy2 - ZWVB0004, ZWVB0015 (SWL)
      *
      *   Rev 2.21   Feb 26 2016 13:37:04   t13ex30
      *FVB00793: SV-Nr in eigenem DB-Feld
      *
      *   Rev 2.20   Nov 25 2015 13:34:02   t13ae30
      *AVB01376: Neue Funktion $34c u. §34i f. Badenia
      *
      *   Rev 2.19   Jan 30 2012 16:30:34   t13ae23
      *AAN01042: fehlende Anlageberatung eingereichtes Investement-
      *Geschäft
      *
      *   Rev 2.18   Sep 30 2011 08:27:48   t13ae13
      *AVB01185: Maßnahmenpaket - Teil 1
      *
      *   Rev 2.17   May 20 2008 12:13:20   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.16   24 Oct 2007 15:12:06   t13ae30
      *AAS00716: Erweiterung Access-DB um Gew.Reg.Nummern
      *
      *   Rev 2.15   16 Aug 2007 09:49:44   t13ex22
      *AVD00044: VBI Release 4.10 Beförderungsvorausschau
      *
      *   Rev 2.14   18 Jul 2007 08:39:36   t13ae13
      *AVB00923: neue Aufstiegsbedingungen DVAG 2007 - MA erweitern
      *
      *   Rev 2.13   16 May 2007 11:21:20   t13ae13
      *AVB00908: Neuer VB-Vertrag 2007 - UPVB0128
      *
      *   Rev 2.12   24 Jan 2007 15:45:42   t13ex57
      *AAN00697: Gewerbeberechtigung bei Antragserfassung DVAG-Wien
      *
      *   Rev 2.11   Jun 08 2006 14:48:42   t13ex43
      *AVB00850: PGM - Autom. Rückstufung von VBA-K zu VBA-S
      *
      *   Rev 2.10   Mar 20 2006 16:24:52   t13ex43
      *ACV00054: PGM - Change Request CTV-Service-Funktion
      *
      *   Rev 2.9   Nov 04 2004 10:35:46   t13ex43
      *AVB00771: Z-PGMe - Steuernummer auf der VB-Abrechnung
      *
      *   Rev 2.8   Jul 01 2004 17:33:04   t13ae30
      *AAS00472: Erweiterung F&A Schnittstelle
      *
      *   Rev 2.7   Feb 20 2003 15:32:50   t13ex22
      *AVB00648: Unterscheidung VBA/ VBA-Karriere
      *
      *   Rev 2.6   Jan 24 2003 11:43:40   t13ex57
      *AAS00387 - DIM 7 Zukunft Nadeln
      *
      *   Rev 2.5   Jun 20 2002 14:24:18   t13ae13
      *AVB00606: Einführung Seminare für DVAG-Wien
      *
      *   Rev 2.4   Apr 30 2002 09:09:06   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.3   22 Oct 2001 11:31:16   t13ae13
      *AVB00534: 2.SST für VB-Portal
      *
      *   Rev 2.2   Dec 13 1999 12:18:06   T13AE42
      *AAS00195 - Einbau Handy-Nr. fuer Hr. Dr. Pohl
      *
      *   Rev 2.1   Aug 24 1999 17:36:24   T13EX19
      *fki00122 - neue fkt drvb0442
      *
      *   Rev 2.0   May 05 1999 17:13:20   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 1.0   Feb 19 1997 09:57:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0143  FUNKTION                           *
      *                           "Lese-OZP zu MA VIVBB004 "         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    06.08.1996                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !06.08.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      *FKI00122 !24.08.99! 02 ! Neue Fkt DRVB0422 wird    !Zgur-Vujic*
      *         !        !    ! eingefügt                 !          *
      *AAS00195 !06.12.99! 03 ! NEUE FKT DRVB0260         !B.SCHMIDT *
      *AVB00534 !19.10.01! 04 ! NEUE FKT DRVB0560         ! Walb     *
      *ACV00007 !13.03.02! 05 ! NEUE FKT DRVB0580         !P.Duerr   *
      *AVB00606 !13.06.02!2.5 ! NEUE FKT DRVB0587         ! Walb     *
      *AAS00387 !20.12.02!2.6 ! NEUE FKT DRVB0629         ! U. BOLL  *
      *AVB00648 !17.02.03!2.7 ! NEUE FKT DRVB0640         ! MUND     *
      *AAS00472 !15.09.04!2.8 ! NEUE FKT DRVB0715         ! WILLIG   *
031104*AVB00771 !03.11.04!2.9 ! DRVB0742 HINZU            ! KLEEMEIER*
200306*ACV00054 !20.03.06!2.10! DRVB0796 HINZU            ! KLEEMEIER*
020606*AVB00850 !02.06.06!2.11! DRVB0826 HINZU            ! KLEEMEIER*
171106*AAN00697 !17.11.06!2.12! NEUE FKT DRVB0839         ! BOLL     *
300307*AAVB0906 !30.03.07!2.13! DRVB0851: TUPEL VIVBH004  !GUTFLEISCH*
      *AAVB0908 !14.05.07!2.14! DRVB0847 neu              ! Walb     *
      *AVB00905 !18.06.07!2.15! EINFÜHRUNG ALLFINANZ      ! WILLIG   *
      *AVB00923 !        !    ! Neue Befbed 2007 für DVAG !          *
      *AVD00044 !        !    ! NEUE BEFBED 2007 FÜR DVAG ! MUND     *
      *AAS00716 !23.10.07!2.16! NEUE FKT DRVB0868         ! WILLIG   *
      *AVB00875 !29.04.08!2.17! NEUE FKT DRVB0901         ! MAESS    *
      *AAVB1185 !07.09.11!2.18! DRVB1030 neu              ! Walb     *
      *AAN01042 !19.12.11!2.19! DRVB1033 NEU              ! RAHNER   *
      *AVB01376 !06.11.15!2.20! NEUE FKT DRVB1125         ! WILLIG   *
      *FVB00793 !26.02.16!2.21! NEUE FKT DRVB1128         ! Wölflik  *
      *AAVB1400 !30.08.16!2.22! DRVB1147 neu              ! Walb     *
      * VLA-988 !12.10.20!2.23! NEUE FUNKTION DRVB01293   ! T13AE30   *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
       01  PI-ZWVB0004.
           05 PI-ZWVB0004-FUNKTION            PIC X(008).
              88 PI-ZWVB0004-DRVB0144             VALUE 'DRVB0144'.
              88 PI-ZWVB0004-DRVB0442             VALUE 'DRVB0442'.
061299        88 PI-ZWVB0004-DRVB0260             VALUE 'DRVB0260'.
191001        88 PI-ZWVB0004-DRVB0560             VALUE 'DRVB0560'.
130302        88 PI-ZWVB0004-DRVB0580             VALUE 'DRVB0580'.
130602        88 PI-ZWVB0004-DRVB0587             VALUE 'DRVB0587'.
201202        88 PI-ZWVB0004-DRVB0629             VALUE 'DRVB0629'.
170203        88 PI-ZWVB0004-DRVB0640             VALUE 'DRVB0640'.
150904        88 PI-ZWVB0004-DRVB0715             VALUE 'DRVB0715'.
031104        88 PI-ZWVB0004-DRVB0742             VALUE 'DRVB0742'.
200306        88 PI-ZWVB0004-DRVB0796             VALUE 'DRVB0796'.
020606        88 PI-ZWVB0004-DRVB0826             VALUE 'DRVB0826'.
171106        88 PI-ZWVB0004-DRVB0839             VALUE 'DRVB0839'.
300307        88 PI-ZWVB0004-DRVB0851             VALUE 'DRVB0851'.
300307        88 PI-ZWVB0004-DRVB0852             VALUE 'DRVB0852'.
140507        88 PI-ZWVB0004-DRVB0847             VALUE 'DRVB0847'.
180607        88 PI-ZWVB0004-DRVB0854             VALUE 'DRVB0854'.
180607        88 PI-ZWVB0004-DRVB0855             VALUE 'DRVB0855'.
150807        88 PI-ZWVB0004-DRVB0862             VALUE 'DRVB0862'.
231007        88 PI-ZWVB0004-DRVB0868             VALUE 'DRVB0868'.
290408        88 PI-ZWVB0004-DRVB0901             VALUE 'DRVB0901'.
070911        88 PI-ZWVB0004-DRVB1030             VALUE 'DRVB1030'.
300112        88 PI-ZWVB0004-DRVB1033             VALUE 'DRVB1033'.
061115        88 PI-ZWVB0004-DRVB1125             VALUE 'DRVB1125'.
260216        88 PI-ZWVB0004-DRVB1128             VALUE 'DRVB1128'.
300816        88 PI-ZWVB0004-DRVB1147             VALUE 'DRVB1147'.
121020        88 PI-ZWVB0004-DRVB1293             VALUE 'DRVB1293'.
      *
 



