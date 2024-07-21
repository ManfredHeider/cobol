      *****************************************************************
      *    COPY-MEMBER: DRVB1310                                      * 
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0015                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    25.05.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *        
      * ---------+--------+-------------------------------+---------- *        
      * VLA-2627 !25.05.21! NEU                           ! KLEEMEIER *        
      *          !        !                               !           *        
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1310 - ERMITTELN SATZ AUS TABELLE MAAN (TUPEL)* 
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1310.
          05 PI-DRVB1310-DATEN.
             10 PI-DRVB1310-MAAN-BEZUGS-NR     PIC S9(09)       BINARY. 
             10 PI-DRVB1310-KNR-MAAN-TYP       PIC S9(04)       BINARY. 
 
       01 PO-DRVB1310.
         03 PO-DRVB1310-RC                     PIC  X(03).
             88 PO-DRVB1310-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1310-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1310-DATEN.
          05 PO-DRVB1310-NI-VIVBD015.
             10 NI-DRVB1310-ELE                PIC S9(04)       COMP
                                               OCCURS 30 TIMES.
          05 PO-DRVB1310-DCL-VIVBD015.
             10 PO-DRVB1310-TUPEL-GESAMT       PIC  X(419).

      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1310   --------------------------* 
      *---------------------------------------------------------------*