      *****************************************************************         
      *    COPY-MEMBER: DRAT0637                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWAT0060                   *         
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    31.05.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-3405 !31.05.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                               *         
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *         
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *         
      *                                                               *         
      *    FUNKTION DRAT0637 - ERMITTELN SATZ AUS TABELLE BRIEFK      *         
      *                                                               *         
      *****************************************************************         
                                                                                
       01 PI-DRAT0637.                                                          
          05 PI-DRAT0637-DATEN.                                                 
             10 PI-DRAT0637-BRIEF              PIC  X(16).                      
                                                                                
       01 PO-DRAT0637.                                                          
         03 PO-DRAT0637-RC                     PIC  X(03).                      
             88 PO-DRAT0637-RC-OK                   VALUE 'OK '.                
             88 PO-DRAT0637-RC-NOT-FOUND            VALUE 'NFD'.                
         03 PO-DRAT0637-DATEN.                                                  
          05 PO-DRAT0637-NI-VIAT0060.                                           
             10 NI-DRAT0637-BRIEF              PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-ONDEMAND    PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-LADON       PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-DRUCK       PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-EMAIL-VB    PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-EMAIL-FREI  PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-OWNER       PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-DRU-IP      PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-DRU-NAME    PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-FORM-DEF    PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-FORMS       PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-GESELL      PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-BEZ         PIC S9(04)       BINARY.         
             10 NI-DRAT0637-BRIEFK-LAST-UPD    PIC S9(04)       BINARY.         
          05 PO-DRAT0637-DCL-VIAT0060.                                          
             10 PO-DRAT0637-BRIEF              PIC  X(16).                      
             10 PO-DRAT0637-BRIEFK-ONDEMAND    PIC  X(01).                      
             10 PO-DRAT0637-BRIEFK-LADON       PIC  X(01).                      
             10 PO-DRAT0637-BRIEFK-DRUCK       PIC  X(01).                      
             10 PO-DRAT0637-BRIEFK-EMAIL-VB    PIC  X(01).                      
             10 PO-DRAT0637-BRIEFK-EMAIL-FREI  PIC  X(80).                      
             10 PO-DRAT0637-BRIEFK-OWNER       PIC  X(40).                      
             10 PO-DRAT0637-BRIEFK-DRU-IP      PIC  X(32).                      
             10 PO-DRAT0637-BRIEFK-DRU-NAME    PIC  X(32).                      
             10 PO-DRAT0637-BRIEFK-FORM-DEF    PIC  X(16).                      
             10 PO-DRAT0637-BRIEFK-FORMS       PIC  X(16).                      
             10 PO-DRAT0637-BRIEFK-GESELL      PIC  X(15).                      
             10 PO-DRAT0637-BRIEFK-BEZ         PIC  X(80).                      
             10 PO-DRAT0637-BRIEFK-LAST-UPD    PIC  X(26).                      
      *---------------------------------------------------------------*         
      *----   ENDE  COPY-MEMBER  DRAT0637   --------------------------*         
      *---------------------------------------------------------------*