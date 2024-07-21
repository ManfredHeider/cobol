      *****************************************************************         
      *    COPY-MEMBER: DRVB1330                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0513                   *         
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: G.BRUDER                                         *         
      *    DATUM:    21.02.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-4138 !21.02.22! NEU                           ! G.BRUDER  *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                               *         
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *         
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *         
      *                                                               *         
      *    FUNKTION DRVB1330 - ERMITTELN SATZ AUS TABELLE CABS        *         
      *                                                               *         
      *****************************************************************         
                                                                                
       01 PI-DRVB1330.                                                          
          05 PI-DRVB1330-DATEN.                                                 
             10 PI-DRVB1330-ST-NR              PIC S9(09)       BINARY.         
             10 PI-DRVB1330-KNR-ZUSTIMM-STAT   PIC S9(04)       BINARY.         
             10 PI-DRVB1330-CABS-DAT-BIS       PIC  X(10).                      
                                                                                
       01 PO-DRVB1330.                                                          
         03 PO-DRVB1330-RC                     PIC  X(03).                      
             88 PO-DRVB1330-RC-OK                   VALUE 'OK '.                
             88 PO-DRVB1330-RC-NOT-FOUND            VALUE 'NFD'.                
         03 PO-DRVB1330-DATEN.                                                  
          05 PO-DRVB1330-NI-VIVB0513.                                           
             10 NI-DRVB1330-ST-NR              PIC S9(04)       BINARY.         
             10 NI-DRVB1330-KNR-ZUSTIMM-STAT   PIC S9(04)       BINARY.         
             10 NI-DRVB1330-CABS-DAT-BIS       PIC S9(04)       BINARY.         
             10 NI-DRVB1330-KTY-ZUSTIMM-STAT   PIC S9(04)       BINARY.         
             10 NI-DRVB1330-CABS-DAT-VON       PIC S9(04)       BINARY.         
             10 NI-DRVB1330-CABS-LAST-UPDATE   PIC S9(04)       BINARY.         
          05 PO-DRVB1330-DCL-VIVB0513.                                          
             10 PO-DRVB1330-ST-NR              PIC S9(09)       BINARY.         
             10 PO-DRVB1330-KNR-ZUSTIMM-STAT   PIC S9(04)       BINARY.         
             10 PO-DRVB1330-CABS-DAT-BIS       PIC  X(10).                      
             10 PO-DRVB1330-KTY-ZUSTIMM-STAT   PIC S9(04)       BINARY.         
             10 PO-DRVB1330-CABS-DAT-VON       PIC  X(26).                      
             10 PO-DRVB1330-CABS-LAST-UPDATE   PIC  X(26).                      
      *---------------------------------------------------------------*         
      *----   ENDE  COPY-MEMBER  DRVB1330   --------------------------*         
      *---------------------------------------------------------------*         
